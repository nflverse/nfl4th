# Builds nfl4th's win probability model for 1st down states (object
# `wp_first_down_model` in R/sysdata.rda). See R/apply_win_prob.R for what the
# model is and how it is used.
#
# Training rows are regulation 1st and 10 and 1st and goal snaps from all
# finished games since 1999. Games that went to overtime are included: a tie at
# the end of regulation is a real state whose outcome is decided in overtime,
# and dropping those games makes late close games look more decided than they
# are. Ties are dropped. Games with a broken running score in the released
# play-by-play are rebuilt with nflfastR first (see the data section).
#
# The recipe follows nflfastR's vegas_wp model. Model choices were made with
# season grouped cross validation (7 folds of whole seasons), see the PR that
# introduced this file for the evaluation.
#
# Run from the package root. Uses all cores, takes about a minute after the
# download. Set WP_FUNCTIONS_ONLY <- TRUE before sourcing to only define the
# recipe (constants, params, wp_model_data(), wp_fit()), as
# data-raw/_bootstrap_models.R does.

library(dplyr)
library(xgboost)
source("R/helpers.R")
source("R/apply_win_prob.R")
# the field goal table behind wpfd_predict()'s field goal floor
fg_prob_df <- local({
  e <- new.env()
  load("R/sysdata.rda", envir = e)
  e$fg_prob_df
})
set.seed(2013)

constants <- list(
  # remaining-time scale of the final margin, sigma * ((t + eps) / 3600)^alpha
  sigma = 13.5,
  alpha = 0.53,
  eps = 10,
  # weight of the score / spread / home part of the offset
  shrink = 0.85,
  # weight of the whole offset: 1, fading linearly to fade_end over the last
  # fade_t seconds of the game
  fade_end = 0.43,
  fade_t = 900,
  # columns of the offset (see wpfd_design()). The possession terms fade by
  # the time needed to get into field goal range, toward the value of a field
  # goal (fg_eoh), as in nflfastR's vegas_wp. There is no timeout term for the
  # end of the game: fit on 1st downs only it came out with the wrong sign
  # (more timeouts lower WP) and cost nothing to drop. The end of the 1st half
  # one has the right sign. nflfastR's term for the other team's field goal
  # chance after it gets the ball back is left out: on 1st downs alone it
  # comes out with the wrong sign and changes nothing. The trees learn the
  # timeouts, with monotone constraints.
  terms = c(
    "sd",
    "spread_t",
    "home_t",
    "const",
    "field",
    "field2",
    "r2h",
    "const_late",
    "to_diff_late1",
    "const_late1",
    "fg_eoh"
  ),
  # number of trees used, by game seconds remaining (linear in between)
  schedule = data.frame(
    t = c(0, 60, 210, 450, 750, 1350, 2250, 3600),
    n = c(1500L, 1500L, 1200L, 200L, 125L, 100L, 100L, 100L)
  )
)
constants$n_trees <- max(constants$schedule$n)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  base_score = 0.5, # the offset is the only intercept
  eta = 0.03,
  max_depth = 6,
  min_child_weight = 22.5,
  subsample = 0.58,
  colsample_bytree = 1.0,
  lambda = 2.39,
  gamma = 3.46,
  # in the order of wpfd_features
  monotone_constraints = c(0, 0, 0, 0, 1, 1, 1, 1, -1, 1, -1),
  nthread = parallel::detectCores()
)

# ---- data -------------------------------------------------------------------
cols <- c(
  "game_id",
  "season",
  "qtr",
  "posteam",
  "defteam",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "result",
  "spread_line",
  "down",
  "ydstogo",
  "yardline_100",
  "play_type",
  "game_seconds_remaining",
  "half_seconds_remaining",
  "score_differential",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "total_home_score",
  "total_away_score"
)

# training rows from play-by-play
wp_model_data <- function(pbp) {
  pbp |>
    filter(!is.na(result), result != 0) |>
    group_by(game_id) |>
    mutate(first_def = first(stats::na.omit(defteam))) |>
    ungroup() |>
    mutate(
      home = as.numeric(posteam == home_team),
      # the team that kicked off to start the game receives the 2nd half
      # kickoff
      receive_2h_ko = as.numeric(qtr <= 2 & posteam == first_def),
      posteam_spread = if_else(home == 1, spread_line, -spread_line),
      winner = if_else(home_score > away_score, home_team, away_team),
      label = as.numeric(posteam == winner)
    ) |>
    filter(
      qtr <= 4,
      # 1st and 10 or 1st and goal
      down == 1,
      ydstogo == 10 | ydstogo == yardline_100,
      !is.na(play_type),
      !is.na(yardline_100),
      !is.na(score_differential),
      !is.na(posteam_spread),
      !is.na(posteam_timeouts_remaining),
      !is.na(defteam_timeouts_remaining)
    )
}

# the model: probit offset, then the trees on top of it
wp_fit <- function(model_data) {
  off_fit <- stats::glm.fit(
    x = wpfd_design(model_data, constants),
    y = model_data$label,
    family = stats::binomial(link = "probit")
  )
  beta <- off_fit$coefficients
  X <- wpfd_tree_matrix(model_data, constants)
  stopifnot(identical(colnames(X), wpfd_features))
  dtrain <- xgb.DMatrix(
    X,
    label = model_data$label,
    base_margin = wpfd_offset(model_data, beta, constants)
  )
  trees <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = constants$n_trees,
    verbose = 0
  )
  # The model is stored as raw UBJSON, which is meant to be stable across
  # xgboost versions (unlike the legacy binary format).
  list(
    trees_raw = xgb.save.raw(trees, raw_format = "ubj"),
    beta = beta,
    constants = constants,
    xgboost_version = as.character(utils::packageVersion("xgboost"))
  )
}

if (!isTRUE(get0("WP_FUNCTIONS_ONLY", ifnotfound = FALSE))) {
  pbp <- purrr::map(
    1999:nflreadr::most_recent_season(),
    \(s) nflreadr::load_pbp(s) |> select(all_of(cols))
  ) |>
    bind_rows()
  # the 2001-2002 JAX home games and 2011_13_DET_NO have the wrong score in the
  # released data (see data-raw/repair_games.R)
  source("data-raw/repair_games.R")
  pbp <- repair_broken_games(pbp)

  model_data <- wp_model_data(pbp)
  cat(
    "training rows:",
    nrow(model_data),
    " games:",
    n_distinct(model_data$game_id),
    "\n"
  )
  wp_first_down_model <- wp_fit(model_data)
  print(round(wp_first_down_model$beta, 4))

  # sanity check: predictions in this session match the stored model
  chk <- model_data[sample(nrow(model_data), 2000), ] |>
    mutate(season = 2025)
  wpfd_cache$boosters <- NULL
  wpfd_cache$inclusive <- NULL
  p_stored <- wpfd_predict(chk, wp_first_down_model)
  stopifnot(all(is.finite(p_stored)), all(p_stored > 0 & p_stored < 1))

  saveRDS(wp_first_down_model, "data-raw/wp_first_down_model.rds")

  # ---- save, keeping the other internal data ----------------------------------
  # R/sysdata.rda holds other objects. Load them and write them back untouched so
  # that only this model changes. Do NOT rebuild or re-serialize anything else
  # here (in particular no xgboost models) because a different xgboost version on
  # the machine running this script would change those bytes too.
  # data-raw/build_sysdata.R rebuilds everything, including this model from
  # data-raw/wp_first_down_model.rds.
  e <- new.env()
  load("R/sysdata.rda", envir = e)
  untouched <- setdiff(ls(e), "wp_first_down_model")
  before <- mget(untouched, envir = e)
  e$wp_first_down_model <- wp_first_down_model
  save(
    list = ls(e),
    file = "R/sysdata.rda",
    envir = e,
    compress = "bzip2",
    version = 2
  )
  # make sure we did not change anything else. fg_model holds environments, which
  # are never identical() after loading, so compare the serialized objects
  e2 <- new.env()
  load("R/sysdata.rda", envir = e2)
  stopifnot(all(vapply(
    untouched,
    function(n) {
      identical(serialize(before[[n]], NULL), serialize(e2[[n]], NULL))
    },
    TRUE
  )))
}
