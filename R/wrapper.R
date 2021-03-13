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
#' @export
#' @examples
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
#'
#' probs <- nfl4th::add_4th_probs(play)
#'
#' dplyr::glimpse(probs)
#' }
add_4th_probs <- function(df) {

  original_df <- df %>% mutate(index = 1 : n())
  modified_df <- original_df

  if (!"type" %in% names(df)) {
    # message("type not found. Assuming an nflfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      prepare_nflfastr_data() %>%
      filter(down == 4)
  }

  # message("Performing final preparation . . .")
  df <- modified_df %>%
    prepare_df()

  if (!"runoff" %in% names(df)) {
    df$runoff <- 0L
  }

  message(glue::glue("Computing probabilities for {nrow(df)} plays. . ."))
  df <- df %>%
    add_probs() %>%
    mutate(play_no = 1 : n()) %>%
    group_by(play_no) %>%
    mutate(
      punt_prob = if_else(is.na(punt_wp), 0, punt_wp),
      max_non_go = max(fg_wp, punt_prob, na.rm = T),
      go_boost = 100 * (go_wp - max_non_go)
    ) %>%
    ungroup() %>%
    select(
      index, go_boost,
      first_down_prob, wp_fail, wp_succeed, go_wp,
      fg_make_prob, miss_fg_wp, make_fg_wp, fg_wp,
      punt_wp
    )

  original_df %>%
    left_join(df, by = c("index")) %>%
    select(-index) %>%
    return()

}

#' Load calculated 4th down probabilities from `nflfastR` data
#'
#' @description Load calculated 4th down probabilities from `nflfastR` data.
#'
#' @param seasons Seasons to load. Must be 2014 and later.
#' @return `nflfastR` data on 4th downs with the `add_4th_probs()` columns added and also the following:
#' \describe{
#' \item{go}{100 if a team went for it on 4th down, 0 otherwise. It's 100 and 0 as a convenience for obtaining percent of times going for it.}
#' }
#' @export
#' @examples
#' \donttest{
#' probs <- load_4th_pbp(2019:2020)
#'
#' dplyr::glimpse(probs)
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
load_4th_pbp <- function(seasons) {

  if (min(seasons) < 2014) {
    stop("Season before 2014 supplied. Please try again with nothing before 2014.")
  }

  # this is less likely to result in crashes due to memory
  purrr::map_df(seasons, ~{
    message(glue::glue("Loading season {.x}"))
    nflfastR::load_pbp(.x) %>%
      nfl4th::add_4th_probs() %>%
      return()
    }) %>%
    dplyr::mutate(
      go = ifelse(
        (rush == 1 | pass == 1) & !play_type_nfl %in% c("PUNT", "FIELD_GOAL"),
        100, 0
      )
    ) %>%
    return()

}

#' Get 2pt decision probabilities
#'
#' @description Get various probabilities associated with each option on PATs (go
#' for it, kick PAT).
#'
#' @param df A data frame of decisions to be computed for.
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
#' @examples
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
#'
#' probs <- nfl4th::add_2pt_probs(play)
#'
#' dplyr::glimpse(probs)
add_2pt_probs <- function(df) {

  original_df <- df %>% mutate(index = 1 : n())
  modified_df <- original_df

  if (!"type" %in% names(df)) {
    message("type not found. Assuming an nflfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      prepare_nflfastr_data() %>%
      filter(
        !is.na(two_point_conv_result) | !is.na(extra_point_result)
      )
  }

  # message("Performing final preparation . . .")
  df <- modified_df %>%
    prepare_df()

  message(glue::glue("Computing probabilities for  {nrow(df)} plays. . ."))
  df <- df %>%
    get_2pt_wp() %>%
    select(
      index,
      wp_0, wp_1, wp_2,
      conv_1pt, conv_2pt,
      wp_go1, wp_go2
    )

  original_df %>%
    left_join(df, by = c("index")) %>%
    select(-index) %>%
    return()

}
