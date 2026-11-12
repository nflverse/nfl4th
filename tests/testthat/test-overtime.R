test_that("overtime rules follow the rule changes", {
  expect_equal(ot_rule(2011, "reg"), "SD")
  expect_equal(ot_rule(c(2012, 2024), "reg"), c("MSD", "MSD"))
  expect_equal(ot_rule(2025, "reg"), "BOTH")
  expect_equal(
    ot_rule(c(2009, 2010, 2021, 2022), "post"),
    c("SD", "MSD", "MSD", "BOTH")
  )
})

test_that("overtime possessions are counted from play-by-play", {
  pbp <- tibble::tibble(
    game_id = "g",
    qtr = c(4, 5, 5, 5, 5, 5, 5, 5, 5),
    posteam = c("A", "B", "B", "B", "B", "A", "A", "B", "B"),
    # kickoff, three snaps and a punt by B, A's possession, B again
    down = c(1, NA, 1, 2, 4, 1, 4, 1, 1)
  )
  expect_equal(
    ot_possession_from_pbp(pbp)$ot_possession,
    c(NA, 1, 1, 1, 1, 2, 2, 3, 3)
  )
})

test_that("a missing overtime possession is guessed from the score", {
  x <- ot_play(score_differential = c(0, -3)) |>
    dplyr::mutate(ot_possession = NA_real_) |>
    dplyr::select(-"ot_rule", -"ot_end")
  expect_warning(y <- prepare_ot(x), "ot_possession")
  expect_equal(y$ot_possession, c(1, 2))
})

test_that("overtime field goals follow the rules", {
  fg <- function(...) get_fg_wp(ot_play(...))
  # sudden death (third possession): a make wins
  expect_equal(fg(ot_possession = 3)$make_fg_wp, 1)
  # second possession down 3: a make ties and the other team gets the ball
  tie <- fg(ot_possession = 2, score_differential = -3)
  expect_gt(tie$make_fg_wp, 0.3)
  expect_lt(tie$make_fg_wp, 0.7)
  # a miss when trailing in the second possession loses
  expect_equal(tie$miss_fg_wp, 0)
  # second possession down 7: a make still loses
  expect_equal(fg(ot_possession = 2, score_differential = -7)$make_fg_wp, 0)
  # first possession, both teams get the ball (2025+): a make doesn't win yet
  first <- fg(ot_possession = 1)
  expect_gt(first$make_fg_wp, 0.6)
  expect_lt(first$make_fg_wp, 0.9)
  # modified sudden death (2024) works the same way for field goals
  expect_lt(fg(season = 2024, ot_possession = 1)$make_fg_wp, 0.9)
})

test_that("overtime punts follow the rules", {
  punt <- function(...) get_punt_wp(ot_play(yardline_100 = 65, ...))$punt_wp
  # a team that trails in the second possession can't punt the game away
  # (only a muffed punt keeps it alive)
  expect_lt(punt(ot_possession = 2, score_differential = -3), 0.01)
  # a punt on the first possession leaves a close game
  first <- punt(ot_possession = 1)
  expect_gt(first, 0.35)
  expect_lt(first, 0.6)
})

test_that("overtime touchdowns follow the rules", {
  td <- function(...) {
    ot_play(...) |>
      dplyr::mutate(score_differential = score_differential + 6) |>
      get_2pt_wp()
  }
  # modified sudden death: a first possession touchdown wins
  expect_equal(td(season = 2024, ot_possession = 1)$wp_td, 1)
  # both teams get the ball: the other team can still answer
  both <- td(season = 2025, ot_possession = 1)
  expect_gt(both$wp_td, 0.75)
  expect_lt(both$wp_td, 1)
  # second possession down 7: the try decides between a tie and a loss
  down7 <- td(season = 2025, ot_possession = 2, score_differential = -7)
  expect_equal(down7$wp_0, 0)
  expect_equal(down7$wp_2, 1)
  expect_gt(down7$wp_1, 0.3)
  expect_lt(down7$wp_1, 0.7)
})

test_that("a regular season overtime that runs out is a tie", {
  # a miss with no time left: the game ends tied
  out <- get_fg_wp(ot_play(ot_possession = 3, quarter_seconds_remaining = 3))
  expect_equal(out$miss_fg_wp, 0.5)
})
