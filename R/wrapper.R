################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Top-Level function made available through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down decision probabilities
#'
#' @description Get various probabilities associated with each option on 4th downs (go
#' for it, kick field goal, punt).
#'
#' @param df A data frame of decisions to be computed for.
#' @param bootstrap If `TRUE`, also value every play under 100 bootstrap
#'   versions of nfl4th's win probability and go for it models (see Details).
#'   The first use downloads them (about 35 MB) to the package cache.
#' @details Overtime plays (`qtr > 4`) are valued with the overtime rules of
#' the game (sudden death, modified sudden death, or both teams getting the
#' ball) using nflfastR's overtime win probability model. `quarter_seconds_remaining`
#' is then the time left in the overtime period. Add `ot_possession` (1 for the
#' first possession of overtime, 2 for the second, 3 for any later possession)
#' to tell which possession it is. Without it, a tied game is assumed to be in
#' the first possession and a team that trails in the second. `nflfastR` data
#' doesn't need it, the possessions are counted from the plays.
#'
#' A conversion that doesn't score is valued at the offense's next snap, at the
#' typical gap after a conversion for the quarter, the time left and the score
#' (about 37 seconds away from the end of a half, as little as 6 to 10 in the
#' last seconds). Add `runoff` to set the seconds that run off after the 6 of
#' the play instead.
#'
#' With `bootstrap = TRUE` each play is also valued under 100 bootstrap
#' versions of the two models nfl4th fits itself, the 1st down win probability
#' model and the go for it model, each refit on a history resampled by game.
#' With the shipped fit that makes 101 draws, the shipped fit first. They show
#' how much a recommendation depends on the particular games the models were
#' fit on (sampling uncertainty, not every source of model error). The punt and
#' field goal models are the same in every draw.
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#' \item{go_boost}{Gain (or loss) in win prob associated with choosing to go for it (percentage points).}
#' \item{first_down_prob}{Probability of earning a first down if going for it on 4th down.}
#' \item{wp_fail}{Win probability in the event of a failed 4th down attempt.}
#' \item{wp_succeed}{Win probability in the event of a successful 4th down attempt.}
#' \item{go_wp}{Average win probability when going for it on 4th down.}
#' \item{fg_make_prob}{Probability of making field goal.}
#' \item{miss_fg_wp}{Win probability in the event of a missed field goal.}
#' \item{make_fg_wp}{Win probability in the event of a made field goal.}
#' \item{fg_wp}{Average win probability when attempting field goal.}
#' \item{punt_wp}{Average win probability when punting.}
#' }
#' With `bootstrap = TRUE`, also:
#' \describe{
#' \item{boot_pct}{Percent of the 101 draws whose best option (go, field goal or punt) is the one with the shipped fit.}
#' \item{boot_go_pct}{Percent of the draws in which going beats the better kick (go_boost > 0). The recommendation itself is the sign of go_boost; this says how often the draws agree that going is better.}
#' \item{go_boost_lo, go_boost_hi}{5th and 95th percentile of go_boost across the draws, a 90% interval.}
#' \item{go_boost_boot}{go_boost in each draw (a list column of length 101 vectors, the shipped fit first).}
#' \item{go_wp_boot, fg_wp_boot, punt_wp_boot}{go_wp, fg_wp and punt_wp in each draw, likewise.}
#' }
#' @export
#' @examplesIf identical(data.table::getDTthreads(FALSE), 2L) || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' \donttest{
#' play <-
#'   tibble::tibble(
#'     # things to help find the right game (use "reg" or "post")
#'     home_team = "GB",
#'     away_team = "TB",
#'     posteam = "GB",
#'     type = "post",
#'     season = 2020,
#'
#'     # information about the situation
#'     qtr = 4,
#'     quarter_seconds_remaining = 129,
#'     ydstogo = 8,
#'     yardline_100 = 8,
#'     score_differential = -8,
#'
#'     home_opening_kickoff = 0,
#'     posteam_timeouts_remaining = 3,
#'     defteam_timeouts_remaining = 3
#'   )
#' try({# to avoid cran issues
#' probs <- nfl4th::add_4th_probs(play)
#'
#' tibble::glimpse(probs)
#' })
#' }
add_4th_probs <- function(df, bootstrap = FALSE) {
  original_df <- df_assign(df, list(index = seq_len(nrow(df))))
  modified_df <- original_df

  if (!"type" %in% names(df)) {
    # message("type not found. Assuming an nflfastR df and doing necessary
    # cleaning . . .")
    modified_df <- prepare_nflfastr_data(original_df)
    modified_df <- df_filter(modified_df, modified_df$down == 4)
  }

  # message("Performing final preparation . . .")
  df <- prepare_df(modified_df)

  # seconds run off after a conversion beyond the 6 of the play; NA uses the
  # typical gap to the next snap (see conversion_runoff())
  if (!"runoff" %in% names(df)) {
    df$runoff <- NA_real_
  }

  cli::cli_alert_info("Computing probabilities for {nrow(df)} plays. . .")
  if (isTRUE(bootstrap) && nrow(df) > 0) {
    # the point estimate comes with the draws
    boot <- add_boot_probs(df)
    df <- boot$point
  } else {
    bootstrap <- FALSE
    df <- add_probs(df)
  }
  punt_prob <- if_else(is.na(df$punt_wp), 0, df$punt_wp)
  # the better kick (punt_prob is never NA)
  max_non_go <- pmax(df$fg_wp, punt_prob, na.rm = TRUE)
  probs <- c(
    list(index = df$index, go_boost = 100 * (df$go_wp - max_non_go)),
    df_cols(df)[c(
      "first_down_prob",
      "wp_fail",
      "wp_succeed",
      "go_wp",
      "fg_make_prob",
      "miss_fg_wp",
      "make_fg_wp",
      "fg_wp",
      "punt_wp"
    )]
  )

  if (isTRUE(bootstrap)) {
    probs <- c(probs, boot$cols)
  }

  out <- df_left_join(original_df, probs, by = "index")
  df_select(out, setdiff(names(out), "index"))
}

#' Load calculated 4th down probabilities from `nflfastR` data
#'
#' @description Load calculated 4th down probabilities from `nflfastR` data.
#'
#' @param seasons Seasons to load. Must be 2014 and later.
#' @param fast Defaults to FALSE. If TRUE, loads pre-computed decisions from repository
#' @return `nflfastR` data on 4th downs with the `add_4th_probs()` columns added and also the following:
#' \describe{
#' \item{go}{100 if a team went for it on 4th down, 0 otherwise. It's 100 and 0 as a convenience for obtaining percent of times going for it.}
#' }
#' @export
#' @examplesIf identical(data.table::getDTthreads(FALSE), 2L) || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' \donttest{
#' try({# Wrap in try to avoid CRAN test problems
#' probs <- load_4th_pbp(2019:2020)
#' tibble::glimpse(probs)
#' })
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
load_4th_pbp <- function(seasons, fast = FALSE) {
  if (min(seasons) < 2014) {
    stop(
      "Season before 2014 supplied. Please try again with nothing before 2014."
    )
  }

  # season-by-season = less likely to result in crashes due to memory
  if (fast) {
    data <- df_bind_rows(lapply(
      seasons,
      function(.x) {
        cli::cli_alert_info("Loading season {.x}")
        df_left_join(
          nflreadr::load_pbp(.x),
          nflreadr::rds_from_url(paste0(
            "https://github.com/nflverse/nfl4th/releases/download/",
            "nfl4th_infrastructure/pre_computed_go_boost.rds?raw=true"
          )),
          by = c("game_id", "play_id")
        )
      }
    ))
  } else {
    data <- df_bind_rows(lapply(
      seasons,
      function(.x) {
        cli::cli_alert_info("Loading season {.x}")
        nflreadr::load_pbp(.x) |>
          nfl4th::add_4th_probs()
      }
    ))
  }

  desc <- data$desc
  go <- ifelse(
    (data$rush == 1 | data$pass == 1) &
      !data$play_type_nfl %in% c("PUNT", "FIELD_GOAL"),
    100,
    0
  )
  # if it's an aborted snap in punt formation, call it a punt
  go <- ifelse(
    data$aborted_play == 1 & re_detect(desc, "Punt formation"),
    0,
    go
  )
  # if it's a run formation or pass formation and there's a dead ball
  # penalty, set go to NA
  # since we can't know the intention of the play
  go <- case_when(
    re_detect(desc, "(Run formation)|(Pass formation)|(Shotgun)") &
      re_detect(desc, "(False Start)|(Neutral Zone Infraction)") ~
      NA_real_,
    TRUE ~ go
  )
  df_assign(data, list(go = go))
}


#' Get 2pt decision probabilities
#'
#' @description Get various probabilities associated with each option on PATs (go
#' for it, kick PAT).
#'
#' @param df A data frame of decisions to be computed for.
#' @details Overtime plays (`qtr > 4`) are valued with the overtime rules of
#' the game (sudden death, modified sudden death, or both teams getting the
#' ball) using nflfastR's overtime win probability model. `quarter_seconds_remaining`
#' is then the time left in the overtime period. Add `ot_possession` (1 for the
#' first possession of overtime, 2 for the second, 3 for any later possession)
#' to tell which possession it is. Without it, a tied game is assumed to be in
#' the first possession and a team that trails in the second. `nflfastR` data
#' doesn't need it, the possessions are counted from the plays.
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#'  first_down_prob, wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, fg_wp, punt_wp
#' \item{wp_0}{Win probability when scoring 0 points on PAT.}
#' \item{wp_1}{Win probability when scoring 1 point on PAT.}
#' \item{wp_2}{Win probability when scoring 2 points on PAT.}
#' \item{conv_1pt}{Probability of making PAT kick.}
#' \item{conv_2pt}{Probability of converting 2-pt attempt.}
#' \item{wp_go1}{Win probability associated with going for 1.}
#' \item{wp_go2}{Win probability associated with going for 2.}
#' }
#' @export
#' @examplesIf identical(data.table::getDTthreads(FALSE), 2L) || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' \donttest{
#' play <-
#'   tibble::tibble(
#'     # things to help find the right game (use "reg" or "post")
#'     home_team = "GB",
#'     away_team = "TB",
#'     posteam = "GB",
#'     type = "post",
#'     season = 2020,
#'
#'     # information about the situation
#'     qtr = 4,
#'     quarter_seconds_remaining = 123,
#'     score_differential = -2,
#'
#'     home_opening_kickoff = 0,
#'     posteam_timeouts_remaining = 3,
#'     defteam_timeouts_remaining = 3
#'   )
#' try({# to avoid cran issues
#' probs <- nfl4th::add_2pt_probs(play)
#'
#' tibble::glimpse(probs)
#' })
#' }
add_2pt_probs <- function(df) {
  original_df <- df_assign(df, list(index = seq_len(nrow(df))))
  modified_df <- original_df

  if (!"type" %in% names(df)) {
    message(
      "type not found. Assuming an nflfastR df and doing necessary ",
      "cleaning . . ."
    )
    modified_df <- prepare_nflfastr_data(original_df)
    modified_df <- df_filter(
      modified_df,
      !is.na(modified_df$two_point_conv_result) |
        !is.na(modified_df$extra_point_result)
    )
  }

  # message("Performing final preparation . . .")
  df <- prepare_df(modified_df)

  cli::cli_alert_info("Computing probabilities for  {nrow(df)} plays. . .")
  df <- get_2pt_wp(df)
  probs <- df_cols(df)[c(
    "index",
    "wp_0",
    "wp_1",
    "wp_2",
    "conv_1pt",
    "conv_2pt",
    "wp_go1",
    "wp_go2"
  )]

  out <- df_left_join(original_df, probs, by = "index")
  df_select(out, setdiff(names(out), "index"))
}
