test_that("punt outcome probabilities sum to 1 at every line of scrimmage", {
  tot <- tapply(punt_df$pct, punt_df$yardline_100, sum)
  expect_equal(as.numeric(names(tot)), 31:99)
  expect_true(all(abs(tot - 1) < 1e-9))
  expect_true(all(punt_df$pct >= 0))
})

test_that("the punt model is smooth along the field", {
  received <- punt_df[punt_df$muff == 0 & punt_df$yardline_after < 100, ]
  start <- tapply(
    received$pct * received$yardline_after,
    received$yardline_100,
    sum
  ) /
    tapply(received$pct, received$yardline_100, sum)
  # the receiving team's average start moves at most about a yard per yard
  expect_lt(max(abs(diff(start))), 1.5)
  expect_lt(mean(abs(diff(diff(start)))), 0.1)
  # touchbacks get likelier closer to the end zone, without jumps
  tb <- vapply(
    31:99,
    \(y) {
      sum(received$pct[
        received$yardline_100 == y & received$yardline_after == 20
      ])
    },
    1
  )
  expect_lt(max(abs(diff(tb))), 0.03)
  expect_gt(tb[1], tb[length(tb)])
})
