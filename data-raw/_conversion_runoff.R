# The gaps in conversion_runoff() (R/helpers.R): mean seconds from the snap of a
# 3rd or 4th down run or pass that gains a first down (not a touchdown, no
# turnover) to the offense's next snap, by seconds left in the half, for the 2nd
# quarter, the 4th quarter with the offense behind or tied, the 4th quarter
# with it ahead, and the 1st and 3rd quarters. 2016 on, regular season and
# postseason. conversion_runoff() takes these means at the middle of each bin,
# linear in between. Run from the package root.

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
  "game_seconds_remaining"
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
    down_next = lead(down)
  ) |>
  ungroup() |>
  filter(
    down %in% 3:4,
    series_result == "First down",
    touchdown == 0,
    fumble_lost == 0,
    interception == 0,
    play_type %in% c("run", "pass"),
    posteam_next == posteam,
    qtr_next == qtr,
    down_next == 1
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
