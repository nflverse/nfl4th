gp_summary <- function(x) {
  gp <- fd_gain_probs(x)
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
fd_play <- function(ydstogo = 1, yardline_100 = 50, season = 2025) {
  data.frame(
    ydstogo = ydstogo,
    yardline_100 = yardline_100,
    season = season,
    posteam_spread = 0,
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
