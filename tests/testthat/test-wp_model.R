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
  # the PAT is a 33 yard field goal
  pat <- wpfd_pat_prob(x)
  expect_true(all(pat > 0.9 & pat < 0.99))
  post <- pat *
    (1 - wpfd_raw(wpfd_post_td_state(x, 7))) +
    (1 - pat) * (1 - wpfd_raw(wpfd_post_td_state(x, 6)))
  expect_true(all(wp <= post + 1e-12))
})

test_that("a team ahead that can kneel out the clock is at 0.9998", {
  # 1st and 10 up 3 with 1:00 left: the defense's 2 timeouts stop the clock
  # twice, so three kneels run off 52 seconds
  x <- wp_state(
    game_seconds_remaining = c(0, 30, 60, 90),
    score_differential = 3,
    yardline_100 = 70,
    defteam_timeouts_remaining = 2
  )
  wp <- wpfd_predict(x)
  expect_true(all(wp[1:3] >= 0.9998))
  # 38 seconds beyond the kneel time the model is nearly back
  expect_lt(wp[4], 0.9998)
})

test_that("a team behind deep in its own half with seconds left rarely wins", {
  x <- wp_state(
    game_seconds_remaining = c(0, 2, 10, 19),
    score_differential = -1,
    yardline_100 = 80
  )
  wp <- wpfd_predict(x)
  expect_equal(wp[1], 0)
  expect_lt(wp[2], 0.02)
  expect_true(all(diff(wp) > 0))
})

test_that("win probability is smooth at the end of the game", {
  # one second steps in the last 4 minutes
  g <- expand.grid(
    t = 1:250,
    sd = c(-8, -3, 0, 3),
    yl = c(5, 35, 60, 95)
  )
  x <- wp_state(
    game_seconds_remaining = g$t,
    score_differential = g$sd,
    yardline_100 = g$yl,
    ydstogo = pmin(10, g$yl)
  )
  g$wp <- wpfd_predict(x)
  g <- g[order(g$sd, g$yl, g$t), ]
  same <- c(FALSE, diff(g$t) == 1)
  # the largest is real: down 3 at the 5 in the last seconds, where each second
  # decides whether a touchdown try still leaves time for the tying kick (about
  # 3 points a second; the clock window narrows to nothing at 0:00)
  expect_lt(max(abs(diff(g$wp))[same[-1]]), 0.04)
  # one yard steps between the 5s, last minute
  g <- expand.grid(yl = 5:95, t = c(3, 8, 15, 30, 60), sd = c(-3, -1, 0))
  g$wp <- wpfd_predict(wp_state(
    game_seconds_remaining = g$t,
    score_differential = g$sd,
    yardline_100 = g$yl,
    ydstogo = pmin(10, g$yl)
  ))
  step <- unlist(lapply(split(g, list(g$t, g$sd)), function(z) diff(z$wp)))
  expect_lt(max(abs(step)), 0.06)
})

test_that("a field goal at the end of the game is valued by the result", {
  # down 3 at the opponent's 10 with 3 seconds left: a make ties the game,
  # worth about the overtime model's coin toss value
  x <- wp_state(
    game_seconds_remaining = 3,
    score_differential = -3,
    yardline_100 = 10
  )
  p <- wpfd_fg_prob(x)
  v <- wpfd_fg_value(x, wp_first_down_model, p)
  expect_equal(v, p * wpfd_ot_start(x), tolerance = 1e-6)
  # down 1, a make wins
  x$score_differential <- -1
  expect_equal(wpfd_fg_value(x, wp_first_down_model, p), p, tolerance = 1e-6)
})

test_that("a tie at the end of regulation moves to its value at 0:00", {
  # tied at the own 35, out of field goal range: over the last 10 seconds WP
  # moves to overtime at the coin toss
  x <- wp_state(game_seconds_remaining = c(0, 5, 9.99, 10), yardline_100 = 65)
  wp <- wpfd_predict(x)
  ot <- wpfd_ot_start(x[1, ])
  expect_equal(wp[1], ot)
  expect_equal(wp[2], 0.5 * wpfd_raw(x[2, ]) + 0.5 * ot)
  # continuous at 10 seconds, where the model takes over
  expect_equal(wp[4], wpfd_raw(x[4, ]))
  expect_lt(abs(wp[3] - wp[4]), 0.005)
  # in field goal range the kick is the better option left
  y <- wp_state(game_seconds_remaining = 0, yardline_100 = 20)
  fg <- wpfd_fg_value(y, wp_first_down_model, wpfd_fg_prob(y))
  expect_gt(fg, wpfd_ot_start(y))
  expect_equal(wpfd_predict(y), fg)
})

test_that("field goal range keeps its value at the end of the 1st half", {
  # 1st and 10 at the opponent's 20 with 10 seconds left is close to a free
  # field goal, while at the own 5 the half is as good as over
  fg_range <- wpfd_predict(wp_state(1810, yardline_100 = 20))
  backed_up <- wpfd_predict(wp_state(1810, yardline_100 = 95))
  expect_gt(fg_range - backed_up, 0.1)
  # and it doesn't fade over the last two minutes
  two_min <- wpfd_predict(wp_state(1920, yardline_100 = 20))
  expect_lt(abs(two_min - fg_range), 0.01)
})

test_that("win probability is at least the value of kicking a field goal", {
  g <- expand.grid(
    t = c(3000, 1920, 1830, 1810, 1803, 600, 30, 3),
    yl = c(5, 20, 35, 45)
  )
  x <- wp_state(
    game_seconds_remaining = g$t,
    yardline_100 = g$yl,
    ydstogo = pmin(10, g$yl)
  )
  fg <- wpfd_fg_value(x, wp_first_down_model, wpfd_fg_prob(x))
  expect_true(all(wpfd_predict(x) >= fg - 1e-12))
  # the make probability comes from the field goal table: outdoors without a
  # roof column, from the 35 in the current era
  expect_equal(
    wpfd_fg_prob(wp_state(yardline_100 = 35, season = 2025)),
    fg_prob_df$fg_make_prob[
      fg_prob_df$yardline_100 == 35 & fg_prob_df$fg_model_roof == "11"
    ]
  )
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
