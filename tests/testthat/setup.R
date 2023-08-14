## Limit datatable to two CPU cores for test purposes
# https://github.com/Rdatatable/data.table/issues/5658

current_threads <- data.table::getDTthreads()
data.table::setDTthreads(2)
withr::defer(data.table::setDTthreads(current_threads), testthat::teardown_env())

play <-
  tibble::tibble(
    # things to help find the right game (use "reg" or "post")
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
