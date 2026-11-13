# R/df_helpers.R (the dplyr verbs nfl4th used) and the ESPN scraper's string
# helpers in R/helpers.R and R/get_game_data.R

test_that("df_assign() replaces in place, appends, recycles size 1 and keeps the class", {
  x <- tibble::tibble(a = 1:3, b = c("x", "y", "z"))
  out <- df_assign(x, list(b = 3:1, c = 0))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("a", "b", "c"))
  expect_identical(out$b, 3:1)
  expect_identical(out$c, c(0, 0, 0))
  expect_error(df_assign(x, list(d = 1:2)))
  # a zero row data frame takes size 1 values
  expect_identical(nrow(df_assign(x[0, ], list(c = 1))), 0L)
})

test_that("df_select() picks and renames by name and keeps the class", {
  x <- tibble::tibble(a = 1, b = 2, c = 3)
  out <- df_select(x, c("c", new_a = "a"))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("c", "new_a"))
  expect_identical(out$new_a, 1)
})

test_that("df_filter() drops NA rows like filter()", {
  x <- data.frame(a = 1:4)
  expect_identical(df_filter(x, c(TRUE, NA, FALSE, TRUE))$a, c(1L, 4L))
  expect_identical(nrow(df_filter(x, rep(FALSE, 4))), 0L)
})

test_that("df_left_join() matches NA to NA, keeps x's rows in order and suffixes clashes", {
  x <- tibble::tibble(k = c(1L, NA, 2L, 3L), v = c("a", "b", "c", "d"))
  y <- data.frame(k = c(2, NA, 2, 9), v = c("y1", "y2", "y3", "y4"), w = 1:4)
  out <- df_left_join(x, y, by = "k")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("k", "v.x", "v.y", "w"))
  # the keys take the common type (double); 2 matches twice, in y's order
  expect_identical(out$k, c(1, NA, 2, 2, 3))
  expect_identical(out$v.x, c("a", "b", "c", "c", "d"))
  expect_identical(out$v.y, c(NA, "y2", "y1", "y3", NA))
  expect_identical(out$w, c(NA, 2L, 1L, 3L, NA))
})

test_that("df_bind_rows() takes the class of the first", {
  a <- tibble::tibble(x = 1:2)
  out <- df_bind_rows(list(a, data.frame(x = 3L, y = "z")))
  expect_s3_class(out, "tbl_df")
  expect_identical(out$x, 1:3)
  expect_identical(out$y, c(NA, NA, "z"))
})

test_that("df_order() is stable, puts NA last and sorts strings in the C locale", {
  expect_identical(df_order(list(c(2, NA, 1, 2))), c(3L, 1L, 4L, 2L))
  expect_identical(df_order(list(c(2, NA, 1, 2)), TRUE), c(1L, 4L, 3L, 2L))
  expect_identical(df_order(list(c("b", "B", "a"))), c(2L, 3L, 1L))
  # two keys, the second descending
  expect_identical(
    df_order(list(c(1, 1, 2, 1), c(5, 7, 1, 6)), c(FALSE, TRUE)),
    c(2L, 4L, 1L, 3L)
  )
})

test_that("df_first_of_groups() is the first row of each group, by key", {
  keys <- list(c("b", "a", "b", "a", NA))
  expect_identical(df_first_of_groups(keys), c(2L, 1L, 5L))
})

test_that("df_rename() renames and refuses missing columns", {
  x <- data.frame(a = 1, b = 2)
  expect_named(df_rename(x, c(z = "b")), c("a", "z"))
  expect_error(df_rename(x, c(z = "c")), "don't exist")
})

test_that("the verbs match dplyr", {
  skip_if_not_installed("dplyr", "1.2.0")
  withr::local_seed(64)
  for (it in 1:50) {
    n <- sample(c(0, 1, 7, 30), 1)
    x <- tibble::tibble(
      k1 = sample(c(1:3, NA), n, TRUE),
      k2 = sample(c("a", "B", "b", NA), n, TRUE),
      v = stats::runif(n)
    )
    m <- sample(c(0, 1, 5, 12), 1)
    y <- tibble::tibble(
      k1 = sample(c(1, 2, 4, NA), m, TRUE),
      k2 = sample(c("a", "b", NA), m, TRUE),
      v = stats::runif(m),
      w = sample(letters, m, TRUE)
    )
    expect_identical(
      df_left_join(x, y, by = c("k1", "k2")),
      dplyr::left_join(
        x,
        y,
        by = c("k1", "k2"),
        relationship = "many-to-many"
      )
    )
    expect_identical(
      df_slice(x, df_order(list(x$k2, x$k1), c(TRUE, FALSE))),
      dplyr::arrange(x, dplyr::desc(k2), k1)
    )
    expect_identical(
      df_slice(x, df_first_of_groups(list(x$k1, x$k2))),
      dplyr::slice(dplyr::group_by(x, k1, k2), 1) |> dplyr::ungroup()
    )
    expect_identical(df_bind_rows(list(x, y)), dplyr::bind_rows(x, y))
    expect_identical(lag1(x$v), dplyr::lag(x$v))
  }
})

test_that("the scraper's string helpers", {
  x <- c("Timeout #2 by KC at 01:58.", NA, "no timeout")
  expect_identical(
    re_extract(x, "(?<=Timeout #[[:digit:]] by )[[:upper:]]{2,3}"),
    c("KC", NA, NA)
  )
  expect_identical(re_detect(x, "Timeout"), c(TRUE, NA, FALSE))
  expect_identical(split_part(c("KC 35", "50", ""), 2L), c("35", NA, NA))
  expect_identical(first_value(c(NA, 2)), NA_real_)
  expect_identical(first_value(numeric(0)), NA_real_)
  expect_named(
    clean_names(data.frame(a.b = 1, periodNumber = 2, a_b = 3)),
    c("a_b", "period_number", "a_b_2")
  )
  expect_identical(
    team_name_fn(c("WSH", "LAR", "OAK", "SD", "LA", "LAC", NA)),
    c("WAS", "LA", "LV", "LAC", "LA", "LAC", NA)
  )
})
