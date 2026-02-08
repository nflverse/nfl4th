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
