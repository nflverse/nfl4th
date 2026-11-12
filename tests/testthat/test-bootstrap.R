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
  # every column of the point estimate has its draws
  for (k in c(
    "first_down_prob",
    "wp_fail",
    "wp_succeed",
    "fg_make_prob",
    "miss_fg_wp",
    "make_fg_wp"
  )) {
    expect_equal(vapply(b[[paste0(k, "_boot")]], `[`, 1, 1), a[[k]])
  }
  expect_true(all(b$boot_median_draw %in% 1:3))
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
  # the workers load the installed nfl4th, which is this version only under
  # R CMD check
  skip_if_not(testthat::is_checking())

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

test_that("each draw is one coherent fit, and the median draw is the median", {
  skip_on_cran()
  skip_if_offline()

  f <- withr::local_tempfile(fileext = ".rds")
  shipped <- list(wp = wp_first_down_model, fd = fd_tables)
  shift <- function(by) {
    d <- shipped
    d$fd$conv <- d$fd$conv + by
    d
  }
  saveRDS(
    list(
      draws = list(shipped, shift(0.3), shift(-0.3), shift(0.1), shift(-0.1))
    ),
    f
  )
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
  b <- suppressMessages(nfl4th::add_4th_probs(play, bootstrap = TRUE))
  for (i in seq_len(nrow(b))) {
    p <- b$first_down_prob_boot[[i]]
    expect_equal(
      b$go_wp_boot[[i]],
      p * b$wp_succeed_boot[[i]] + (1 - p) * b$wp_fail_boot[[i]]
    )
    m <- b$fg_make_prob_boot[[i]]
    expect_equal(
      b$fg_wp_boot[[i]],
      m * b$make_fg_wp_boot[[i]] + (1 - m) * b$miss_fg_wp_boot[[i]]
    )
    j <- b$boot_median_draw[i]
    expect_equal(b$go_boost_boot[[i]][j], stats::median(b$go_boost_boot[[i]]))
  }
  # the shifted conversion tables move first_down_prob
  expect_gt(b$first_down_prob_boot[[1]][2], b$first_down_prob_boot[[1]][3])
})
