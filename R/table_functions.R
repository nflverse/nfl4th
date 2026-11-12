#' Get 4th down decision probabilities
#'
#' @description Get a table with the probabilities on 4th down.
#'
#' @param probs A data frame consisting of one play that has had `add_4th_probs()` already run on it.
#' @return A table showing the probabilities associated with each possible choice.
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
#' nfl4th::make_table_data(probs)
#' })
#'}
make_table_data <- function(probs) {
  choice_table(list(
    choice_rows(
      "Go for it",
      probs$go_wp,
      probs$first_down_prob,
      probs$wp_fail,
      probs$wp_succeed
    ),
    choice_rows(
      "Field goal attempt",
      probs$fg_wp,
      probs$fg_make_prob,
      probs$miss_fg_wp,
      probs$make_fg_wp
    ),
    choice_rows(
      "Punt",
      if_else(is.na(probs$punt_wp), NA_real_, probs$punt_wp),
      NA_real_,
      NA_real_,
      NA_real_
    )
  ))
}


#' Get 2pt decision probabilities
#'
#' @description Get a table with the probabilities associated with a 2-pt decision.
#'
#' @param probs A data frame consisting of one play that has had `add_2pt_probs()` already run on it.
#' @return A table showing the probabilities associated with each possible choice.
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
#' nfl4th::make_2pt_table_data(probs)
#' })
#' }
make_2pt_table_data <- function(probs) {
  choice_table(list(
    choice_rows(
      "Go for 2",
      probs$wp_go2,
      probs$conv_2pt,
      probs$wp_0,
      probs$wp_2
    ),
    choice_rows(
      "Kick XP",
      probs$wp_go1,
      probs$conv_1pt,
      probs$wp_0,
      probs$wp_1
    )
  ))
}

# one choice's rows of the table
choice_rows <- function(
  choice,
  choice_prob,
  success_prob,
  fail_wp,
  success_wp
) {
  tibble::tibble(
    choice = choice,
    choice_prob = choice_prob,
    success_prob = success_prob,
    fail_wp = fail_wp,
    success_wp = success_wp
  )
}

# the choices stacked, probabilities in percent
choice_table <- function(rows) {
  out <- df_bind_rows(rows)
  pct <- c("choice_prob", "success_prob", "fail_wp", "success_wp")
  df_assign(out, lapply(df_cols(out)[pct], function(v) 100 * v))
}
