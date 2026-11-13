test_that("add_probs() reads only the columns in decision_cols", {
  # add_probs() drops every column not in decision_cols before valuing the
  # options, so a column the code reads must be listed. On nflfastR
  # play-by-play (every column it has), the decisions must come out the same
  # with all the columns
  skip_on_cran()
  skip_if_offline()

  pbp <- nflreadr::load_pbp(2025)
  p <- prepare_nflfastr_data(df_assign(pbp, list(index = seq_len(nrow(pbp)))))
  p <- df_filter(p, p$down == 4)
  # late game and overtime plays take the most paths through the code
  withr::local_seed(4)
  late <- which(p$game_seconds_remaining <= 300 | p$qtr > 4)
  rows <- sort(unique(c(
    sample(late, min(40, length(late))),
    sample(nrow(p), 40)
  )))
  df <- prepare_df(df_slice(p, rows))
  df$runoff <- NA_real_
  expect_true(ncol(df) > 300)

  narrow <- add_probs(df)
  wide <- vctrs::new_data_frame(df_cols(df), n = nrow(df)) |>
    get_go_wp() |>
    get_fg_wp() |>
    get_punt_wp()
  for (k in c(boot_columns, "first_down_prob")) {
    expect_identical(narrow[[k]], wide[[k]], label = k)
  }
})

test_that("the states after a decision use the WP model's kneel rule", {
  skip_on_cran()
  skip_if_offline()
  # 1st and 10 up 3 with 1:00 left and no timeouts for the trailing team:
  # whoever has the ball kneels out the clock. The state's WP is for the team
  # with the 4th down decision (GB)
  s <- prepare_df(play)
  s$down <- 1
  s$ydstogo <- 10
  s$yardline_100 <- 70
  s$game_seconds_remaining <- s$half_seconds_remaining <- 60
  s$home_timeouts_remaining <- s$away_timeouts_remaining <- 0
  s$score_differential <- 3
  s <- s[c(1, 1), ]
  s$posteam <- c("GB", "TB")
  expect_identical(decision_state_wp(s)$state_wp, c(1, 0))
})

test_that("the shared helpers", {
  expect_identical(
    other_team(c("KC", "BUF", NA), "KC", "BUF"),
    c("BUF", "KC", NA)
  )
  d <- data.frame(home_timeouts_remaining = 3, away_timeouts_remaining = 1)
  expect_identical(timeouts_home_away(d, c(TRUE, FALSE)), c(3, 1))
  expect_error(get_fd_era(c(2020, NA)), "must not contain missing values")
})
