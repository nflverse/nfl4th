test_that("nfl4th loads tibble (nflreadr data frames are also data.tables)", {
  # without tibble's methods, subsetting nflreadr play-by-play follows
  # data.table's rules
  expect_true("tibble" %in% names(getNamespaceImports("nfl4th")))
})

test_that("go_boost and the best option work on matrices of draws", {
  go <- matrix(c(0.5, 0.2, 0.6, 0.1), 2)
  fg <- matrix(c(0.4, NA, 0.7, NA), 2)
  punt <- matrix(c(NA, 0.3, NA, 0.05), 2)
  expect_equal(
    boot_go_boost(go, fg, punt),
    100 * (go - matrix(c(0.4, 0.3, 0.7, 0.05), 2))
  )
  expect_equal(boot_decision(go, fg, punt), matrix(c(1L, 3L, 2L, 1L), 2))
})

test_that("a draw's models are used only inside with_draw()", {
  other <- list(wp = list(trees_raw = as.raw(1:3)), fd = list(conv = 1))
  with_draw(other, {
    expect_identical(active_wp_model(), other$wp)
    expect_identical(active_fd_tables(), other$fd)
  })
  expect_identical(active_wp_model(), wp_first_down_model)
  expect_identical(active_fd_tables(), fd_tables)
  # also after an error
  try(with_draw(other, stop("x")), silent = TRUE)
  expect_identical(active_wp_model(), wp_first_down_model)
})

test_that("add_4th_probs(bootstrap = TRUE) adds the draws", {
  skip_on_cran()
  skip_if_offline()

  # three draws that are all the shipped fit: every draw agrees with it
  f <- withr::local_tempfile(fileext = ".rds")
  shipped <- list(wp = wp_first_down_model, fd = fd_tables)
  saveRDS(list(draws = list(shipped, shipped, shipped)), f)
  withr::local_options(nfl4th.bootstrap_models = f)
  play <- tibble::tibble(
    home_team = "GB",
    away_team = "TB",
    posteam = "GB",
    type = "post",
    season = 2020,
    qtr = 4,
    quarter_seconds_remaining = c(129, 600),
    ydstogo = c(8, 4),
    yardline_100 = c(8, 45),
    score_differential = c(-8, 3),
    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
  a <- suppressMessages(nfl4th::add_4th_probs(play))
  b <- suppressMessages(nfl4th::add_4th_probs(play, bootstrap = TRUE))
  # the point estimate doesn't change
  expect_identical(b[names(a)], a)
  expect_equal(b$boot_pct, c(100, 100))
  expect_equal(b$boot_go_pct, 100 * (a$go_boost > 0))
  expect_equal(b$go_boost_lo, a$go_boost)
  expect_equal(b$go_boost_hi, a$go_boost)
  expect_equal(lengths(b$go_boost_boot), c(3, 3))
  expect_equal(vapply(b$go_boost_boot, `[`, 1, 1), a$go_boost)
  expect_equal(vapply(b$fg_wp_boot, `[`, 1, 3), a$fg_wp)
})

test_that("draws built for other models are refused", {
  f <- withr::local_tempfile(fileext = ".rds")
  other <- list(wp = list(trees_raw = as.raw(1:3)), fd = fd_tables)
  saveRDS(list(draws = list(other, other)), f)
  withr::local_options(nfl4th.bootstrap_models = f)
  expect_error(bootstrap_models(), "built for other models")
})

test_that("worker processes give the same draws", {
  skip_on_cran()
  skip_if_offline()
  # the workers load the installed nfl4th
  skip_if_not(nzchar(system.file(package = "nfl4th")))

  # three different draws: the shipped fit, and the shipped fit with its go
  # for it tables shifted (any change will do)
  f <- withr::local_tempfile(fileext = ".rds")
  shipped <- list(wp = wp_first_down_model, fd = fd_tables)
  other <- shipped
  other$fd$conv <- other$fd$conv + 0.3
  saveRDS(list(draws = list(shipped, other, shipped)), f)
  withr::local_options(nfl4th.bootstrap_models = f)
  play <- tibble::tibble(
    home_team = "GB",
    away_team = "TB",
    posteam = "GB",
    type = "post",
    season = 2020,
    qtr = 4,
    quarter_seconds_remaining = c(129, 600),
    ydstogo = c(8, 4),
    yardline_100 = c(8, 45),
    score_differential = c(-8, 3),
    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
  here <- suppressMessages(nfl4th::add_4th_probs(play, bootstrap = TRUE))
  nfl4th::nfl4th_start_workers(2)
  withr::defer(nfl4th::nfl4th_stop_workers())
  there <- suppressMessages(nfl4th::add_4th_probs(play, bootstrap = TRUE))
  expect_identical(there, here)
  # the shifted tables convert more often
  expect_true(all(vapply(here$go_wp_boot, `[`, 1, 2) > here$go_wp))
})
