test_that("add_4th_probs works", {
  # This test requires model download which isn't safe on CRAN
  skip_on_cran()
  skip_if_offline()

  one_play <- tibble::tibble(
    # things to help find the right game (use "reg" or "post" for type)
    home_team = "GB",
    away_team = "TB",
    posteam = "GB",
    type = "post",
    season = 2020,

    # information about the situation
    qtr = 4,
    quarter_seconds_remaining = 129,
    ydstogo = 8,
    yardline_100 = 8,
    score_differential = -8,

    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )

  expect_snapshot({
    one_play |>
      nfl4th::add_4th_probs() |>
      nfl4th::make_table_data() |>
      knitr::kable(digits = 2)
  })
})

test_that("add_2pt_probs works", {
  # Models required for this test are stored inside the package so we run it
  # on CRAN to catch breaking changes in dependencies like dplyr or xgboost

  another_play <- tibble::tibble(
    # things to help find the right game (use "reg" or "post")
    home_team = "GB",
    away_team = "TB",
    posteam = "GB",
    type = "post",
    season = 2020,

    # information about the situation
    qtr = 4,
    quarter_seconds_remaining = 123,
    score_differential = -2,

    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )

  expect_snapshot(
    {
      another_play |>
        nfl4th::add_2pt_probs() |>
        nfl4th::make_2pt_table_data() |>
        knitr::kable(digits = 2)
    },
    cran = TRUE
  )
})

test_that("a conversion is valued at the next snap, and runoff moves it", {
  skip_on_cran()
  skip_if_offline()

  # down 3 with 1:10 left, 4th & 3 at the TB 40: the clock runs after a
  # conversion, so less time is left for the drive than 6 seconds after the snap
  play <- tibble::tibble(
    home_team = "GB",
    away_team = "TB",
    posteam = "GB",
    type = "post",
    season = 2020,
    qtr = 4,
    quarter_seconds_remaining = 70,
    ydstogo = 3,
    yardline_100 = 40,
    score_differential = -3,
    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
  go <- function(runoff) {
    play$runoff <- runoff
    suppressMessages(nfl4th::add_4th_probs(play))$go_wp
  }
  typical <- suppressMessages(nfl4th::add_4th_probs(play))$go_wp
  # runoff moves the clock (before, the clock was clamped per play with max(),
  # which undid it)
  expect_gt(go(0) - go(60), 0.05)
  # the typical gap here is about 17 seconds
  expect_lte(typical, go(0) + 1e-3)
  expect_gt(typical, go(60))
})

test_that("the gap to the next snap after a conversion", {
  # away from the end of a half the clock runs about 37 seconds
  expect_equal(conversion_runoff(1, 1500, 0), 36.5, tolerance = 0.01)
  expect_gt(conversion_runoff(4, 600, 0), 30)
  # late in a half teams hurry or stop the clock
  expect_lt(conversion_runoff(4, 30, -3), 15)
  expect_lt(conversion_runoff(2, 30, 7), 12)
  # a later conversion never leads to an earlier next snap
  h <- 0:900
  for (q in c(2, 4)) {
    for (sd in c(-3, 0, 3)) {
      expect_true(all(diff(h - conversion_runoff(q, h, sd)) >= 0))
    }
  }
  # without a timeout the clock often runs out after a late conversion
  expect_true(all(diff(conversion_clock_out(0:60)) <= 0))
  expect_gt(conversion_clock_out(3), 0.8)
  expect_equal(conversion_clock_out(60), 0)
  # the play itself takes about 6 seconds, longer for longer gains
  expect_equal(conversion_play_time(0), 6, tolerance = 0.01)
  expect_gt(conversion_play_time(40), conversion_play_time(5))
})

test_that("the clock after a late conversion depends on timeouts", {
  skip_on_cran()
  skip_if_offline()

  # down 3, 4th & 1 at the TB 40
  probs <- function(secs, timeouts) {
    play <- tibble::tibble(
      home_team = "GB",
      away_team = "TB",
      posteam = "GB",
      type = "post",
      season = 2020,
      qtr = 4,
      quarter_seconds_remaining = secs,
      ydstogo = 1,
      yardline_100 = 40,
      score_differential = -3,
      home_opening_kickoff = 0,
      posteam_timeouts_remaining = timeouts,
      defteam_timeouts_remaining = 1
    )
    suppressMessages(nfl4th::add_4th_probs(play))
  }
  # 3 seconds left: the play runs out the clock with or without a timeout, and
  # regulation ends with the team still down 3 unless the play scores. A state
  # at 0:00 is the final result, not a snap that could still kick
  a <- probs(3, 0)
  expect_equal(a$wp_succeed, probs(3, 3)$wp_succeed)
  expect_lt(a$go_wp, 0.03)
  expect_lt(a$go_wp, a$fg_wp)
  # 10 seconds left: a timeout stops the clock after a conversion, leaving time
  # for a kick; without one the clock often runs out
  expect_gt(probs(10, 1)$wp_succeed - probs(10, 0)$wp_succeed, 0.1)
  # a timeout never hurts
  for (s in c(10, 20, 45, 90)) {
    expect_gte(probs(s, 2)$go_wp, probs(s, 0)$go_wp - 1e-9)
  }
})
