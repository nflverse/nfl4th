test_that("win probability is smooth in the clock", {
  t <- seq(3590, 1810, by = -10)
  wp <- wpfd_predict(wp_state(game_seconds_remaining = t, posteam_spread = 7))
  expect_lt(mean(abs(diff(diff(wp)))), 0.002)
  expect_lt(max(abs(diff(wp))), 0.01)
})

test_that("win probability is monotone where it should be", {
  base <- wp_state(
    game_seconds_remaining = rep(c(3300, 2000, 900, 400, 100), each = 4),
    score_differential = rep(c(-7, -3, 0, 3), times = 5),
    posteam_spread = 3,
    yardline_100 = 60
  )
  wp <- wpfd_predict(base)
  up <- function(col, by) {
    x <- base
    x[[col]] <- x[[col]] + by
    wpfd_predict(x)
  }
  expect_true(all(up("score_differential", 1) >= wp))
  expect_true(all(up("posteam_spread", 1) >= wp))
  expect_true(all(up("yardline_100", -1) >= wp))
  x <- base
  x$posteam_timeouts_remaining <- 2
  expect_true(all(wp >= wpfd_predict(x)))
})

test_that("win probability rises with field position on 1st down", {
  wp <- wpfd_predict(wp_state(yardline_100 = 99:1))
  expect_true(all(diff(wp) >= 0))
  # and does so smoothly
  expect_lt(max(abs(diff(diff(wp)))), 0.01)
})

test_that("a team can't be worse off after scoring a touchdown", {
  x <- wp_state(
    game_seconds_remaining = c(3500, 2500, 1500, 800),
    yardline_100 = 1,
    ydstogo = 1
  )
  wp <- wpfd_predict(x)
  post <- 0.94 *
    (1 - wpfd_raw(wpfd_post_td_state(x, 7))) +
    0.06 * (1 - wpfd_raw(wpfd_post_td_state(x, 6)))
  expect_true(all(wp <= post + 1e-12))
})

test_that("the xgboost iteration range convention is detected", {
  b <- wpfd_booster()
  expect_type(wpfd_inclusive_range(b), "logical")
  # with the detected convention, all trees reproduce the full model
  X <- wpfd_tree_matrix(wp_state(), wp_first_down_model$constants)
  n <- wp_first_down_model$constants$n_trees
  full <- as.numeric(stats::predict(
    b,
    xgboost::xgb.DMatrix(X),
    outputmargin = TRUE
  ))
  expect_equal(wpfd_tree_margin(b, X, n, wpfd_inclusive_range(b)), full)
})

test_that("kickoffs start at the typical first snap of each era", {
  expect_equal(
    kickoff_yardline(c(2014, 2016, 2023, 2024, 2025, 2026)),
    c(78L, 75L, 75L, 70L, 69L, 69L)
  )
})
