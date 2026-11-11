# Win probability of the 1st down states that follow a 4th down decision
#
# Every state nfl4th values after a decision is a 1st down: 1st and 10 (or
# goal) for the offense after a conversion, for the other team after a turnover
# on downs, a punt or a missed field goal, and after the kickoff that follows a
# score. In regulation these are valued with nfl4th's own model, trained on
# regulation 1st and 10 and 1st and goal snaps. It follows the recipe of
# nflfastR's vegas_wp model:
#
# 1. offset: a probit model of the final margin in the spirit of Stern (1994).
#    Every term is divided by the remaining-time scale of the margin,
#    sigma * ((t + eps) / 3600) ^ alpha. The score differential, spread and
#    home terms are weighted by `shrink` so the monotone trees can correct
#    their slope in the direction they are allowed to.
# 2. fade(t): the whole offset fades over the last minutes so the trees can
#    learn the end game staircase in score differential.
# 3. trees: xgboost trained on top of the offset with monotone constraints. The
#    number of trees used depends on the clock: few early in the game, where
#    labels are noisy, and many in the last minutes. Margins are interpolated
#    between knots so WP is continuous in time.
# 4. A team can't be worse off after scoring a touchdown on this snap: with more
#    than five minutes left, WP is capped at the value of a TD + PAT.
#
# In regulation nfl4th averages this model with nflfastR's vegas_wp. Overtime
# states are valued with nflfastR's overtime model (see R/overtime.R).
#
# Built by data-raw/_wp_model.R and stored in R/sysdata.rda as
# `wp_first_down_model` (list with the xgboost model as raw UBJSON, the probit
# coefficients and the constants).

wpfd_features <- c(
  "r2h_first_half",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "score_differential",
  "posteam_spread",
  "stern_z",
  "spread_z",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# terms of the probit offset weighted by `shrink`
wpfd_score_terms <- c("sd", "spread_t", "home_t")

# remaining-time scale of the final margin
wpfd_tscale <- function(g, k) {
  k$sigma * ((g + k$eps) / 3600)^k$alpha
}

# feature matrix for the trees. `x` needs game_seconds_remaining,
# half_seconds_remaining, score_differential, posteam_spread, home,
# receive_2h_ko, yardline_100 and both timeouts columns
wpfd_tree_matrix <- function(x, k) {
  g <- x$game_seconds_remaining
  s <- wpfd_tscale(g, k)
  spread_rem <- x$posteam_spread * g / 3600
  X <- cbind(
    r2h_first_half = x$receive_2h_ko * as.numeric(g > 1800),
    home = x$home,
    half_seconds_remaining = x$half_seconds_remaining,
    game_seconds_remaining = g,
    score_differential = x$score_differential,
    posteam_spread = x$posteam_spread,
    stern_z = (x$score_differential + spread_rem) / s,
    spread_z = spread_rem / s,
    yardline_100 = x$yardline_100,
    posteam_timeouts_remaining = x$posteam_timeouts_remaining,
    defteam_timeouts_remaining = x$defteam_timeouts_remaining
  )
  storage.mode(X) <- "double"
  X
}

# columns of the probit offset (all divided by the time scale)
wpfd_design <- function(x, k) {
  g <- x$game_seconds_remaining
  h <- x$half_seconds_remaining
  s <- wpfd_tscale(g, k)
  tau <- g / 3600
  yl <- x$yardline_100
  # possession value fades when the half runs out. The time needed to use a
  # possession grows with the distance to the end zone
  left <- pmin(h / (k$left_a * yl + k$left_b), 1)
  late <- exp(-g / 300)
  to_diff <- x$posteam_timeouts_remaining - x$defteam_timeouts_remaining
  fp <- (50 - yl) / 50
  Z <- cbind(
    sd = x$score_differential,
    spread_t = x$posteam_spread * tau,
    home_t = x$home * tau,
    const = left,
    field = fp * left,
    field2 = fp^2 * left,
    field3 = fp^3 * left,
    r2h = x$receive_2h_ko * as.numeric(g > 1800),
    to_diff_late = to_diff * late,
    const_late = late
  )
  Z[, k$terms, drop = FALSE] / s
}

# weight of the whole offset
wpfd_fade <- function(g, k) {
  ifelse(g >= k$fade_t, 1, k$fade_end + (1 - k$fade_end) * g / k$fade_t)
}

# offset on the logit scale
wpfd_offset <- function(x, beta, k) {
  Z <- wpfd_design(x, k)
  s <- colnames(Z) %in% wpfd_score_terms
  eta <- k$shrink *
    drop(Z[, s, drop = FALSE] %*% beta[s]) +
    drop(Z[, !s, drop = FALSE] %*% beta[!s])
  p <- stats::pnorm(eta)
  wpfd_fade(x$game_seconds_remaining, k) *
    stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}

wpfd_cache <- new.env(parent = emptyenv())

wpfd_booster <- function(model = wp_first_down_model) {
  if (is.null(wpfd_cache$booster)) {
    tryCatch(
      wpfd_cache$booster <- xgboost::xgb.load.raw(model$trees_raw),
      error = function(e) {
        cli::cli_abort(c(
          "Can't load the win probability model.",
          "i" = paste(
            "It was saved with xgboost {.val {model$xgboost_version}} and you",
            "have {.val {as.character(utils::packageVersion('xgboost'))}}."
          ),
          "x" = conditionMessage(e)
        ))
      }
    )
  }
  wpfd_cache$booster
}

# xgboost changed `iterationrange` from a half open [start, end) range (1.x) to
# an inclusive one (3.x). Find out which one the installed version uses by
# comparing the first n_trees rounds with the full model (they are the same
# model only if the range is inclusive).
wpfd_inclusive_range <- function(booster, model = wp_first_down_model) {
  if (is.null(wpfd_cache$inclusive)) {
    k <- model$constants
    x <- data.frame(
      game_seconds_remaining = c(3000, 1500, 400, 90),
      half_seconds_remaining = c(1200, 1500, 400, 90),
      score_differential = c(0, 3, -4, 7),
      posteam_spread = c(-3, 6.5, 2.5, -1),
      home = c(1, 0, 1, 0),
      receive_2h_ko = c(1, 0, 0, 0),
      yardline_100 = c(75, 42, 18, 3),
      posteam_timeouts_remaining = c(3, 2, 1, 0),
      defteam_timeouts_remaining = c(3, 3, 2, 1)
    )
    dm <- xgboost::xgb.DMatrix(wpfd_tree_matrix(x, k))
    full <- stats::predict(booster, dm, outputmargin = TRUE)
    upto <- stats::predict(
      booster,
      dm,
      outputmargin = TRUE,
      iterationrange = c(1L, k$n_trees)
    )
    wpfd_cache$inclusive <- isTRUE(all.equal(
      as.numeric(full),
      as.numeric(upto)
    ))
  }
  wpfd_cache$inclusive
}

# margin of the first n trees. The model is stored with base_score 0.5, so the
# margin has no intercept of its own.
wpfd_tree_margin <- function(booster, X, n, inclusive) {
  if (n <= 0) {
    return(rep(0, nrow(X)))
  }
  as.numeric(stats::predict(
    booster,
    xgboost::xgb.DMatrix(X),
    outputmargin = TRUE,
    iterationrange = c(1L, if (inclusive) n else n + 1L)
  ))
}

# WP of the team with the ball, without the touchdown cap
wpfd_raw <- function(x, model = wp_first_down_model) {
  k <- model$constants
  n <- nrow(x)
  if (n == 0) {
    return(numeric(0))
  }
  booster <- wpfd_booster(model)
  inclusive <- wpfd_inclusive_range(booster, model)
  X <- wpfd_tree_matrix(x, k)
  sch <- k$schedule[order(k$schedule$t), ]
  g <- pmin(pmax(x$game_seconds_remaining, min(sch$t)), max(sch$t))
  j <- pmin(findInterval(g, sch$t), nrow(sch) - 1L)
  j[is.na(j)] <- 1L
  w <- (g - sch$t[j]) / (sch$t[j + 1L] - sch$t[j])
  ns <- unique(sch$n)
  F <- lapply(ns, function(nt) wpfd_tree_margin(booster, X, nt, inclusive))
  names(F) <- as.character(ns)
  tm <- numeric(n)
  for (jj in seq_len(nrow(sch) - 1L)) {
    i <- which(j == jj)
    a <- F[[as.character(sch$n[jj])]][i]
    b <- F[[as.character(sch$n[jj + 1L])]][i]
    tm[i] <- (1 - w[i]) * a + w[i] * b
  }
  stats::plogis(wpfd_offset(x, model$beta, k) + tm)
}

# The state of the next possession after the offense scores `pts` on this snap:
# the other team has 1st and 10 at the typical start after a kickoff, a few
# seconds later
wpfd_post_td_state <- function(x, pts, dt = 6) {
  y <- x
  y$score_differential <- -(x$score_differential + pts)
  y$posteam_spread <- -x$posteam_spread
  y$home <- 1 - x$home
  y$ydstogo <- 10
  y$yardline_100 <- kickoff_yardline(x$season)
  y$posteam_timeouts_remaining <- x$defteam_timeouts_remaining
  y$defteam_timeouts_remaining <- x$posteam_timeouts_remaining
  first <- x$game_seconds_remaining > 1800
  y$receive_2h_ko <- ifelse(first, 1 - x$receive_2h_ko, 0)
  y$game_seconds_remaining <- pmax(x$game_seconds_remaining - dt, 0)
  y$half_seconds_remaining <- pmax(x$half_seconds_remaining - dt, 0)
  y
}

# WP of the team with the ball (regulation). WP is capped at the value of
# scoring a touchdown on this snap, except in the last five minutes (where not
# scoring can be the better play) and in the last seconds of a half (where the
# next possession is not a kickoff return).
wpfd_predict <- function(x, model = wp_first_down_model, pat = 0.94, dt = 6) {
  x <- as.data.frame(x)
  w <- wpfd_raw(x, model)
  cap <- which(
    x$game_seconds_remaining > 300 &
      x$half_seconds_remaining > dt &
      !is.na(w)
  )
  if (length(cap) > 0) {
    xc <- x[cap, , drop = FALSE]
    post <- pat *
      (1 - wpfd_raw(wpfd_post_td_state(xc, 7, dt), model)) +
      (1 - pat) * (1 - wpfd_raw(wpfd_post_td_state(xc, 6, dt), model))
    w[cap] <- pmin(w[cap], post)
  }
  w
}

drop.me <- c("vegas_wp", "vegas_home_wp", "ep")

# Win probability of the states in `pbp_data`, from the point of view of the
# team that has the 4th down decision (`original_posteam`). Regulation states
# are the average of nfl4th's model above and nflfastR's vegas_wp, overtime
# states come from nflfastR's overtime model. Rows can carry `ot_end`, the final
# WP (again for `original_posteam`) of states where the overtime rules end the
# game.
calculate_win_probability <- function(pbp_data) {
  pbp_data <- pbp_data |> dplyr::select(-dplyr::any_of(drop.me))
  pbp_data <- pbp_data |> dplyr::select(-ends_with("_prob"))
  if (nrow(pbp_data) == 0) {
    pbp_data$vegas_wp <- numeric(0)
    return(pbp_data)
  }

  x <- wp_inputs(pbp_data)
  fastr <- fastr_wp(x)

  wp <- rep(NA_real_, nrow(x))
  ot <- x$qtr > 4
  reg <- which(!ot)
  if (length(reg) > 0) {
    wp[reg] <- (wpfd_predict(x[reg, , drop = FALSE]) + fastr[reg]) / 2
  }
  if (any(ot)) {
    wp[ot] <- ot_win_probability(x[ot, , drop = FALSE], fastr[ot])
  }

  # from the point of view of the team with the 4th down decision
  wp <- ifelse(x$posteam != x$original_posteam, 1 - wp, wp)
  if ("ot_end" %in% names(x)) {
    wp <- ifelse(!is.na(x$ot_end), x$ot_end, wp)
  }
  pbp_data$vegas_wp <- wp
  pbp_data
}

# columns the win probability models need, from the point of view of the team
# with the ball
wp_inputs <- function(pbp_data) {
  pbp_data |>
    dplyr::mutate(
      home = as.numeric(posteam == home_team),
      posteam_spread = dplyr::if_else(home == 1, spread_line, -spread_line),
      receive_2h_ko = dplyr::case_when(
        # 1st half, home team opened game with kickoff, away team has ball
        qtr <= 2 & home_opening_kickoff == 1 & posteam == away_team ~ 1,
        # 1st half, away team opened game with kickoff, home team has ball
        qtr <= 2 & home_opening_kickoff == 0 & posteam == home_team ~ 1,
        TRUE ~ 0
      ),
      posteam_timeouts_remaining = dplyr::if_else(
        home == 1,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        home == 1,
        away_timeouts_remaining,
        home_timeouts_remaining
      )
    )
}

# nflfastR's win probability for the team with the ball: vegas_wp in
# regulation, the overtime model in overtime. `x` comes from wp_inputs()
fastr_wp <- function(x) {
  nflfastR::calculate_win_probability(data.frame(
    receive_2h_ko = x$receive_2h_ko,
    home_team = x$home_team,
    posteam = x$posteam,
    score_differential = x$score_differential,
    half_seconds_remaining = x$half_seconds_remaining,
    game_seconds_remaining = x$game_seconds_remaining,
    spread_line = x$spread_line,
    down = x$down,
    ydstogo = x$ydstogo,
    yardline_100 = x$yardline_100,
    posteam_timeouts_remaining = x$posteam_timeouts_remaining,
    defteam_timeouts_remaining = x$defteam_timeouts_remaining,
    qtr = x$qtr,
    season = x$season,
    season_type = toupper(x$type),
    ot_possession = x$ot_possession,
    roof = x$roof,
    stringsAsFactors = FALSE
  ))$vegas_wp
}

# nflfastR's win probability of the states in `pbp_data` for the team with the
# ball, without the average with nfl4th's own model (see fd_late())
fastr_win_probability <- function(pbp_data) {
  if (nrow(pbp_data) == 0) {
    return(numeric(0))
  }
  fastr_wp(wp_inputs(pbp_data))
}
