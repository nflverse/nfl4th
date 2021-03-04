################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Top-Level function made available through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down decision probs
#'
#' @description Get various probabilities associated with each option on 4th downs (
#' go for it, kick field goal, punt).
#'
#' @param df A data frame of decisions to be computed for.
#' @details To load valid game_ids please use the package function
#' \code{\link{fast_scraper_schedules}} (the function can directly handle the
#' output of that function)
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#'  first_down_prob, wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, fg_wp, punt_wp
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
      prepare_nflfastr_data()
  }

  message("Performing final preparation . . .")
  df <- modified_df %>%
    prepare_df()

  message(glue::glue("Computing probabilities for  {nrow(df)} plays. . ."))
  df <- df %>%
    add_probs() %>%
    select(
      index,
      first_down_prob, wp_fail, wp_succeed, go_wp,
      fg_make_prob, miss_fg_wp, make_fg_wp, fg_wp,
      punt_wp
    )

  original_df %>%
    left_join(df, by = c("index")) %>%
    select(-index) %>%
    return()

}
