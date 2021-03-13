#' Get 4th down decision probabilities
#'
#' @description Get a table with the probabilities on 4th down.
#'
#' @param probs A data frame consisting of one play that has had `add_4th_probs()` already run on it.
#' @return A table showing the probabilities associated with each possible choice.
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
#' nfl4th::make_table_data(probs)
#'}
make_table_data <- function(probs) {

  go <- tibble::tibble(
    "choice_prob" = probs$go_wp,
    "choice" = "Go for it",
    "success_prob" = probs$first_down_prob,
    "fail_wp" = probs$wp_fail,
    "success_wp" = probs$wp_succeed
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  fg <- tibble::tibble(
    "choice_prob" = probs$fg_wp,
    "choice" = "Field goal attempt",
    "success_prob" = probs$fg_make_prob,
    "fail_wp" = probs$miss_fg_wp,
    "success_wp" = probs$make_fg_wp
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  punt <- tibble::tibble(
    "choice_prob" = if_else(is.na(probs$punt_wp), NA_real_, probs$punt_wp),
    "choice" = "Punt",
    "success_prob" = NA_real_,
    "fail_wp" = NA_real_,
    "success_wp" = NA_real_
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  for_return <- bind_rows(
    go, fg, punt
  ) %>%
    mutate(
      choice_prob = 100 * choice_prob,
      success_prob = 100 * success_prob,
      fail_wp = 100 * fail_wp,
      success_wp = 100 * success_wp
    )

  return(for_return)
}


#' Get 2pt decision probabilities
#'
#' @description Get a table with the probabilities associated with a 2-pt decision.
#'
#' @param probs A data frame consisting of one play that has had `add_2pt_probs()` already run on it.
#' @return A table showing the probabilities associated with each possible choice.
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
#' nfl4th::make_2pt_table_data(probs)
#'
make_2pt_table_data <- function(probs) {

  go <- tibble::tibble(
    "choice_prob" = probs$wp_go2,
    "choice" = "Go for 2",
    "success_prob" = probs$conv_2pt,
    "fail_wp" = probs$wp_0,
    "success_wp" = probs$wp_2
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  pat <- tibble::tibble(
    "choice_prob" = probs$wp_go1,
    "choice" = "Kick XP",
    "success_prob" = probs$conv_1pt,
    "fail_wp" = probs$wp_0,
    "success_wp" = probs$wp_1
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  for_return <- bind_rows(
    go, pat
  ) %>%
    mutate(
      choice_prob = 100 * choice_prob,
      success_prob = 100 * success_prob,
      fail_wp = 100 * fail_wp,
      success_wp = 100 * success_wp
    )

  return(for_return)
}
