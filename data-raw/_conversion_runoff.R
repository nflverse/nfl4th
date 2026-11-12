# The gaps in conversion_runoff() (R/helpers.R): mean seconds from the snap of a
# 3rd or 4th down run or pass that gains a first down (not a touchdown, no
# turnover) to the offense's next snap when the offense doesn't call a timeout
# in between, by seconds left in the half, for the 2nd quarter, the 4th quarter
# with the offense behind or tied, the 4th quarter with it ahead, and the 1st
# and 3rd quarters. 2016 on, regular season and postseason.
# conversion_runoff() takes these means at the middle of each bin, linear in
# between.
#
# conversion_clock_out(): the share of late conversions (2nd quarter, or 4th
# quarter behind or tied) by a team without a timeout after which the clock ran
# out before another snap, by seconds left in the half. These are missing from
# the gaps above, which only see conversions that were followed by a snap.
#
# And conversion_play_time(): how long a play that gains a first down takes,
# by yards gained, from plays the offense stopped with a timeout before its next
# snap (in bounds, last 2:00 of a half, any down), so the clock ran only during
# the play. A team with a timeout can take that instead of the gap (see
# get_go_wp()).
#
# Run from the package root.

library(dplyr)

cols <- c(
  "game_id",
  "play_id",
  "qtr",
  "down",
  "posteam",
  "play_type",
  "series_result",
  "touchdown",
  "fumble_lost",
  "interception",
  "score_differential",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "posteam_timeouts_remaining",
  "yards_gained",
  "first_down_rush",
  "first_down_pass",
  "out_of_bounds",
  "penalty",
  "qb_spike",
  "qb_kneel"
)
pbp <- purrr::map(
  2016:nflreadr::most_recent_season(),
  \(s) nflreadr::load_pbp(s) |> select(all_of(cols))
) |>
  bind_rows()

gaps <- pbp |>
  filter(!is.na(down), !is.na(posteam), qtr <= 4) |>
  arrange(game_id, play_id) |>
  group_by(game_id) |>
  mutate(
    t_next = lead(game_seconds_remaining),
    posteam_next = lead(posteam),
    qtr_next = lead(qtr),
    down_next = lead(down),
    to_next = lead(posteam_timeouts_remaining)
  ) |>
  ungroup()

# how long a play that gains a first down takes: stopped by the offense's
# timeout before the next snap, in bounds, in the last 2:00 of a half
play_time <- gaps |>
  filter(
    play_type %in% c("run", "pass"),
    first_down_rush %in% 1 | first_down_pass %in% 1,
    touchdown == 0,
    fumble_lost == 0,
    interception == 0,
    !penalty %in% 1,
    qb_spike == 0,
    qb_kneel == 0,
    !out_of_bounds %in% 1,
    half_seconds_remaining <= 120,
    posteam_next == posteam,
    qtr_next == qtr,
    to_next < posteam_timeouts_remaining
  ) |>
  mutate(gap = game_seconds_remaining - t_next) |>
  filter(gap >= 0, gap <= 30)
cat("play time, first down plays stopped by a timeout:", nrow(play_time), "\n")
print(round(coef(lm(gap ~ yards_gained, data = play_time)), 3))

# late conversions without a timeout: did the clock run out before a snap?
clock_out <- gaps |>
  filter(
    down %in% 3:4,
    play_type %in% c("run", "pass"),
    first_down_rush %in% 1 | first_down_pass %in% 1,
    touchdown == 0,
    fumble_lost == 0,
    interception == 0,
    !penalty %in% 1,
    qtr == 2 | (qtr == 4 & score_differential <= 0),
    posteam_timeouts_remaining == 0,
    half_seconds_remaining <= 45
  ) |>
  mutate(
    # no snap by the offense later in the same quarter (row by row)
    ran_out = is.na(posteam_next) |
      is.na(qtr_next) |
      posteam_next != posteam |
      qtr_next != qtr,
    left = cut(half_seconds_remaining, c(-1, 7, 10, 15, 20, 30, 45))
  ) |>
  group_by(left) |>
  summarise(
    conversions = n(),
    share = round(mean(ran_out), 3),
    ran_out = sum(ran_out),
    .groups = "drop"
  )
print(as.data.frame(clock_out))

gaps <- gaps |>
  filter(
    down %in% 3:4,
    series_result == "First down",
    touchdown == 0,
    fumble_lost == 0,
    interception == 0,
    play_type %in% c("run", "pass"),
    posteam_next == posteam,
    qtr_next == qtr,
    down_next == 1,
    # without a timeout in between (a team that has one can take
    # conversion_play_time() instead)
    !to_next < posteam_timeouts_remaining
  ) |>
  mutate(gap = game_seconds_remaining - t_next) |>
  filter(gap >= 0, gap < 200) |>
  mutate(
    group = case_when(
      qtr %in% c(1, 3) ~ "Q1/Q3",
      qtr == 2 ~ "Q2",
      score_differential <= 0 ~ "Q4 behind or tied",
      TRUE ~ "Q4 ahead"
    ),
    left = cut(
      half_seconds_remaining,
      c(
        -1,
        15,
        30,
        45,
        60,
        90,
        120,
        150,
        180,
        240,
        300,
        360,
        480,
        600,
        900,
        1800
      )
    )
  )

gaps |>
  group_by(group, left) |>
  summarise(
    conversions = n(),
    mean_gap = round(mean(gap), 1),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = group,
    values_from = c(mean_gap, conversions)
  ) |>
  as.data.frame() |>
  print()
