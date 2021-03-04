################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Top-Level function made available through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down decision probs
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
add_4th_probs <- function(df) {

  original_df <- df %>% mutate(index = 1 : n())
  modified_df <- original_df

  if (!"home_opening_kickoff" %in% names(df)) {
    message("home_opening_kickoff not found. Assuming an nflfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      prepare_nflfastr_data() %>%
      filter(down == 4)
  }

  message("Performing final preparation . . .")
  df <- modified_df %>%
    prepare_df()

  if (!"runoff" %in% names(df)) {
    df$runoff <- 0L
  }

  message(glue::glue("Computing probabilities for  {nrow(df)} plays. . ."))
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

#' Get 2pt decision probs
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
add_2pt_probs <- function(df) {

  original_df <- df %>% mutate(index = 1 : n())
  modified_df <- original_df

  if (!"home_opening_kickoff" %in% names(df)) {
    message("home_opening_kickoff not found. Assuming an nflfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      prepare_nflfastr_data() %>%
      filter(
        !is.na(two_point_conv_result) | !is.na(extra_point_result)
      )
  }

  message("Performing final preparation . . .")
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
