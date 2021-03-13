source("helper.R")

test_that("Calculate one play: GB", {

  probs <- nfl4th::add_4th_probs(play)

  # positive go boost
  testthat::expect_gt(probs$go_boost, 0)

})

test_that("Make the table: GB", {

  probs <- nfl4th::add_4th_probs(play)
  table <- nfl4th::make_table_data(probs)

  fg_row <- table %>% filter(choice == "Field goal attempt")
  go_row <- table %>% filter(choice == "Go for it")

  # succeeding is better than failing
  testthat::expect_gt(go_row$success_wp, go_row$fail_wp)
  testthat::expect_gt(fg_row$success_wp, fg_row$fail_wp)

})
