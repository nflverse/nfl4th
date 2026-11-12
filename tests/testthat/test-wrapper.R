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
  expect_equal(conversion_runoff(1, 1500, 0), 36.6)
  expect_gt(conversion_runoff(4, 600, 0), 30)
  # late in a half teams hurry or stop the clock
  expect_lt(conversion_runoff(4, 30, -3), 15)
  expect_lt(conversion_runoff(2, 30, 7), 10)
  # a later conversion never leads to an earlier next snap
  h <- 0:900
  for (q in c(2, 4)) {
    for (sd in c(-3, 0, 3)) {
      expect_true(all(diff(h - conversion_runoff(q, h, sd)) >= 0))
    }
  }
})
