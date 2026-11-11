gp_summary <- function(x, late = FALSE) {
  gp <- fd_gain_probs(x, late = late)
  rows <- split(seq_len(nrow(gp)), gp$row)
  data.frame(
    total = vapply(rows, \(j) sum(gp$prob[j]), 1),
    conv = vapply(
      seq_along(rows),
      \(i) {
        j <- rows[[i]]
        sum(gp$prob[j][gp$gain[j] >= x$ydstogo[i]])
      },
      1
    ),
    td = vapply(
      seq_along(rows),
      \(i) {
        j <- rows[[i]]
        sum(gp$prob[j][gp$gain[j] >= x$yardline_100[i]])
      },
      1
    ),
    worst = vapply(seq_along(rows), \(i) min(gp$gain[rows[[i]]]), 1)
  )
}
fd_play <- function(ydstogo = 1, yardline_100 = 50, season = 2025, spread = 0) {
  data.frame(
    ydstogo = ydstogo,
    yardline_100 = yardline_100,
    season = season,
    posteam_spread = spread,
    total_line = 45
  )
}

test_that("gain probabilities are a distribution", {
  x <- fd_play(
    ydstogo = c(1, 4, 10, 25, 3, 5),
    yardline_100 = c(50, 20, 75, 80, 3, 96)
  )
  s <- gp_summary(x)
  expect_equal(s$total, rep(1, nrow(x)))
  # goal to go: a conversion is a touchdown
  expect_equal(s$conv[5], s$td[5])
  # a failed attempt never ends behind the own 1
  expect_gte(min(s$worst - (x$yardline_100 - 99)), 0)
})

test_that("conversion probability falls with distance and is smooth in field position", {
  x <- fd_play(ydstogo = 1:15, yardline_100 = 60)
  conv <- gp_summary(x)$conv
  expect_true(all(diff(conv) < 0))
  expect_gt(conv[1], 0.6)
  expect_lt(conv[15], 0.3)
  f <- fd_play(ydstogo = 8, yardline_100 = 98:9)
  cf <- gp_summary(f)$conv
  expect_lt(mean(abs(diff(diff(cf)))), 0.001)
})

test_that("touchdowns get likelier closer to the end zone", {
  td <- gp_summary(fd_play(ydstogo = 2, yardline_100 = 40:3))$td
  expect_true(all(diff(td) > 0))
})

test_that("future seasons use the current era", {
  expect_equal(
    get_fd_era(c(2001, 2019, 2020, 2025, 2040)),
    c(0L, 0L, 1L, 1L, 1L)
  )
  expect_equal(
    gp_summary(fd_play(season = 2040)),
    gp_summary(fd_play(season = 2025))
  )
  expect_error(get_fd_era(NA), "season")
})

test_that("the era shift depends on distance", {
  # since 2020 4th downs convert more often at short distances, not at long ones
  shift <- function(t) {
    stats::qlogis(gp_summary(fd_play(ydstogo = t, season = 2025))$conv) -
      stats::qlogis(gp_summary(fd_play(ydstogo = t, season = 2015))$conv)
  }
  expect_gt(shift(2), 0.1)
  expect_lt(shift(12), 0)
})

test_that("the spread matters less on 4th and long", {
  # favorites convert 4th and short more often, but not 4th and long
  edge <- function(t) {
    stats::qlogis(gp_summary(fd_play(ydstogo = t, spread = 7))$conv) -
      stats::qlogis(gp_summary(fd_play(ydstogo = t, spread = 0))$conv)
  }
  expect_gt(edge(1), 0.1)
  expect_lt(edge(10), 0.6 * edge(1))
})

test_that("late and desperate 4th and long converts more often than normal", {
  # 2001-2026 4th and 8-10: 38.2% late or desperate, 28.7% otherwise
  x <- fd_play(ydstogo = c(1, 10), yardline_100 = 50)
  normal <- gp_summary(x)$conv
  late <- gp_summary(x, late = TRUE)$conv
  expect_gt(normal[1], 0.65)
  expect_lt(normal[2], 0.33)
  expect_gt(late[2], normal[2] + 0.07)
  expect_lt(abs(late[1] - normal[1]), 0.05)
})

test_that("late means trailing late in the game or a low WP", {
  # the end of the 1st half isn't late
  late <- fd_is_late(
    qtr = c(4, 4, 4, 2, 1, 5, 3, NA),
    score_differential = c(-3, 3, -3, 7, 0, -3, 0, 0),
    game_seconds_remaining = c(500, 500, 700, 1830, 3000, 300, 1200, NA),
    wp = c(0.4, 0.9, 0.4, 0.6, 0.5, 0.4, 0.1, NA)
  )
  expect_equal(late, c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})
