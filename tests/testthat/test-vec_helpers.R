# R/vec_helpers.R: if_else(), case_when() and coalesce() with dplyr's semantics

test_that("if_else() keeps NA and takes the common type", {
  expect_identical(
    if_else(c(TRUE, FALSE, NA), 1L, c(2.5, 3.5, 4.5)),
    c(1, 3.5, NA)
  )
  expect_identical(if_else(c(TRUE, FALSE), "a", NA), c("a", NA))
  expect_error(if_else(c(TRUE, FALSE), 1, "a"))
  expect_error(if_else(c(TRUE, FALSE), 1:3, 1))
})

test_that("case_when() takes the first TRUE condition, NA counts as FALSE", {
  x <- c(1, 5, NA, 10)
  expect_identical(
    case_when(x < 3 ~ "low", x < 8 ~ "mid", TRUE ~ "high"),
    c("low", "mid", "high", "high")
  )
  # no default: NA where no condition holds
  expect_identical(case_when(x < 3 ~ 1L, x > 8 ~ 2L), c(1L, NA, NA, 2L))
  # conditions and values recycle from size 1
  expect_identical(case_when(TRUE ~ x), x)
  expect_identical(case_when(x > 2 ~ 0, TRUE ~ x), c(1, 0, NA, 0))
})

test_that("coalesce() fills each missing value from the next argument", {
  expect_identical(coalesce(c(1, NA, NA), c(NA, 2, NA), 0), c(1, 2, 0))
  expect_identical(coalesce(c(NA, 1L), 2.5), c(2.5, 1))
  expect_identical(coalesce(c(NA, "a"), "b"), c("b", "a"))
})

test_that("if_else(), case_when() and coalesce() match dplyr", {
  # the helpers make the calls dplyr 1.2 makes; a newer dplyr could differ
  skip_if_not_installed("dplyr", "1.2.0")
  withr::local_seed(11)
  gen <- function(n, type) {
    x <- switch(
      type,
      lgl = sample(c(TRUE, FALSE, NA), n, TRUE),
      int = sample(c(-3:3, NA), n, TRUE),
      dbl = sample(c(-1.5, 0, 2.25, NA, NaN, Inf), n, TRUE),
      chr = sample(c("a", "b", "", NA), n, TRUE),
      fct = factor(sample(c("a", "b", NA), n, TRUE), levels = c("a", "b")),
      na = rep(NA, n)
    )
    if (stats::runif(1) < 0.15 && n > 0) {
      names(x) <- paste0("n", seq_len(n))
    }
    x
  }
  types <- c("lgl", "int", "dbl", "chr", "fct", "na")
  # the same value, or both an error
  same <- function(a, b) {
    a <- tryCatch(a, error = function(e) "ERROR")
    b <- tryCatch(b, error = function(e) "ERROR")
    identical(a, b)
  }
  bad <- character()
  for (it in 1:1000) {
    n <- sample(c(0, 1, 2, 5, 13), 1)
    size <- function() if (stats::runif(1) < 0.3) 1 else n
    t1 <- sample(types, 1)
    t2 <- if (stats::runif(1) < 0.6) t1 else sample(types, 1)
    t3 <- if (stats::runif(1) < 0.7) t1 else sample(types, 1)
    cond <- gen(n, "lgl")
    x <- gen(size(), t1)
    y <- gen(size(), t2)
    z <- gen(size(), t3)
    c1 <- gen(size(), "lgl")
    c2 <- gen(n, "lgl")
    what <- paste(it, t1, t2, t3, n)
    ok <- c(
      if_else = same(dplyr::if_else(cond, x, y), if_else(cond, x, y)),
      coalesce3 = same(dplyr::coalesce(x, y, z), coalesce(x, y, z)),
      coalesce2 = same(dplyr::coalesce(x, y), coalesce(x, y)),
      case_when_default = same(
        dplyr::case_when(c1 ~ x, c2 ~ y, TRUE ~ z),
        case_when(c1 ~ x, c2 ~ y, TRUE ~ z)
      ),
      case_when = same(
        dplyr::case_when(c1 ~ x, c2 ~ y),
        case_when(c1 ~ x, c2 ~ y)
      )
    )
    if (!all(ok)) {
      bad <- c(bad, paste(names(ok)[!ok], what))
    }
  }
  expect_identical(bad, character())
})
