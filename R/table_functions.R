# get the data from a situation into a table
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


# get the numbers that go into the table
# this is a separate function in case one wants the actual numbers
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

  # more debugging
  # global_data <<- for_return

  return(for_return)
}
