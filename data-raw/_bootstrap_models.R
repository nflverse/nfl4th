# Builds the bootstrap draws behind add_4th_probs(bootstrap = TRUE): 101
# versions of the two models nfl4th fits itself, the 1st down win probability
# model (data-raw/_wp_model.R) and the go for it model
# (data-raw/_go_for_it_model.R). Draw 1 is the shipped fit; draws 2 to 101 refit
# both on a history resampled by game: the games are drawn with replacement, as
# many as there are, and a game drawn k times contributes its rows k times to
# both models. The punt and field goal tables and nflfastR's models stay fixed.
#
# Why resample whole games (phi = 1, no fractional bootstrap): win/loss labels
# are shared by every snap of a game, and on these models the game bootstrap
# matches the actual sampling spread. Fitting both models on two random halves
# of the games (10 splits), the bootstrap of a half predicted the half-vs-half
# spread of go_boost to within 12% (ratio 0.88, se 0.04) and of FG - punt to
# within 3%, with 90% intervals holding 91% of the time. Drawing half the games
# (phi = 0.5, as Brill, Yurko and Wyner do) overstated it about 1.8 times.
#
# Writes data-raw/bootstrap/draw_<b>.rds as it goes (rerunning skips the draws
# already there), then data-raw/bootstrap_models.rds (38 MB). That file is
# uploaded to the nfl4th_infrastructure release under the name in
# `bootstrap_file` (R/bootstrap.R), which bootstrap_models() downloads; it
# checks that draw 1 is the shipped fit, so rebuilding the models means
# rebuilding the draws under a new name. Needs the nflfastR that repairs the
# broken games (see data-raw/repair_games.R). Run from the package root; about
# 35 seconds per draw.

library(dplyr)
library(mgcv)
library(xgboost)
WP_FUNCTIONS_ONLY <- TRUE
source("data-raw/_wp_model.R")
FD_FUNCTIONS_ONLY <- TRUE
source("data-raw/_go_for_it_model.R")

n_draws <- 101
out_dir <- "data-raw/bootstrap"
dir.create(out_dir, showWarnings = FALSE)

# the shipped fit is draw 1
shipped <- new.env()
load("R/sysdata.rda", envir = shipped)

# ---- data: both models' training rows, with the broken games repaired -------
pbp <- purrr::map(
  1999:nflreadr::most_recent_season(),
  \(s) nflreadr::load_pbp(s) |> select(any_of(union(cols, fd_cols)))
) |>
  bind_rows()
source("data-raw/repair_games.R")
pbp <- repair_broken_games(pbp)
wp_rows <- wp_model_data(pbp)
go_rows <- fd_model_data(pbp)
rm(pbp)
games <- sort(union(unique(wp_rows$game_id), unique(go_rows$game_id)))
cat(
  "WP rows:",
  nrow(wp_rows),
  "| go rows:",
  nrow(go_rows),
  "| games:",
  length(games),
  "\n"
)
idx_wp <- split(seq_len(nrow(wp_rows)), wp_rows$game_id)
idx_go <- split(seq_len(nrow(go_rows)), go_rows$game_id)
# the rows of the drawn games, once per draw of a game
take <- function(d, idx, drawn) {
  d[
    unlist(idx[drawn[drawn %in% names(idx)]], use.names = FALSE),
    ,
    drop = FALSE
  ]
}

# ---- the draws ----------------------------------------------------------------
for (b in 2:n_draws) {
  f <- file.path(out_dir, sprintf("draw_%03d.rds", b))
  if (file.exists(f)) {
    next
  }
  set.seed(64000 + b)
  drawn <- sample(games, length(games), replace = TRUE)
  t0 <- proc.time()[["elapsed"]]
  wp <- wp_fit(take(wp_rows, idx_wp, drawn))
  # the go for it model's late flag uses this draw's WP model, as the shipped
  # fit uses the shipped one
  go_b <- take(go_rows, idx_go, drawn)
  go_b$late_wp <- fd_state_wp(go_b, wp)
  fd <- fd_tabulate(fd_fit(go_b))
  saveRDS(list(wp = wp, fd = fd, games = drawn), f)
  cat(sprintf(
    "draw %d: %d distinct games, %.0f s\n",
    b,
    length(unique(drawn)),
    proc.time()[["elapsed"]] - t0
  ))
}

# ---- one file ----------------------------------------------------------------
draws <- c(
  list(list(wp = shipped$wp_first_down_model, fd = shipped$fd_tables)),
  lapply(2:n_draws, function(b) {
    d <- readRDS(file.path(out_dir, sprintf("draw_%03d.rds", b)))
    d[c("wp", "fd")]
  })
)
bootstrap_models <- list(
  draws = draws,
  # what the draws were made from
  meta = list(
    built = format(Sys.time(), tz = "UTC", usetz = TRUE),
    nfl4th = system("git rev-parse --short HEAD", intern = TRUE),
    xgboost_version = as.character(utils::packageVersion("xgboost")),
    games = length(games),
    wp_rows = nrow(wp_rows),
    go_rows = nrow(go_rows),
    seeds = 64000 + 2:n_draws
  )
)
saveRDS(bootstrap_models, "data-raw/bootstrap_models.rds", compress = "xz")
cat(
  "wrote data-raw/bootstrap_models.rds,",
  round(file.size("data-raw/bootstrap_models.rds") / 2^20, 1),
  "MB\n"
)
