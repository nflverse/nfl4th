pbp <- nfl4th::load_4th_pbp(seq(2014, nflreadr::most_recent_season()))

condensed <- pbp |>
  dplyr::filter(!is.na(go_boost)) |>
  dplyr::select(
    game_id, play_id, go_boost, go_wp, punt_wp, fg_wp
  )

nflversedata::nflverse_save(
  condensed,
  file_name = "pre_computed_go_boost",
  nflverse_type = "nfl4th go boost",
  file_types = "rds",
  repo = "nflverse/nfl4th",
  release_tag = "nfl4th_infrastructure"
)

