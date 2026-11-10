# a 1st and 10 state for the win probability model, from the point of view of
# the team with the ball
wp_state <- function(
  game_seconds_remaining = 2700,
  half_seconds_remaining = NULL,
  score_differential = 0,
  posteam_spread = 0,
  home = 1,
  receive_2h_ko = 0,
  yardline_100 = 75,
  ydstogo = 10,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3,
  season = 2025
) {
  if (is.null(half_seconds_remaining)) {
    half_seconds_remaining <- ifelse(
      game_seconds_remaining > 1800,
      game_seconds_remaining - 1800,
      game_seconds_remaining
    )
  }
  data.frame(
    game_seconds_remaining = game_seconds_remaining,
    half_seconds_remaining = half_seconds_remaining,
    score_differential = score_differential,
    posteam_spread = posteam_spread,
    home = home,
    receive_2h_ko = receive_2h_ko,
    yardline_100 = yardline_100,
    ydstogo = ydstogo,
    posteam_timeouts_remaining = posteam_timeouts_remaining,
    defteam_timeouts_remaining = defteam_timeouts_remaining,
    season = season
  )
}

# a prepared 4th down state for the decision functions (as after prepare_df())
ot_play <- function(
  season = 2025,
  type = "reg",
  score_differential = 0,
  ot_possession = 1,
  yardline_100 = 30,
  ydstogo = 4,
  quarter_seconds_remaining = 400
) {
  tibble::tibble(
    home_team = "DAL",
    away_team = "NYG",
    posteam = "DAL",
    type = type,
    season = season,
    qtr = 5,
    quarter_seconds_remaining = quarter_seconds_remaining,
    ydstogo = ydstogo,
    yardline_100 = yardline_100,
    score_differential = score_differential,
    home_opening_kickoff = 1,
    posteam_timeouts_remaining = 2,
    defteam_timeouts_remaining = 2,
    ot_possession = ot_possession,
    # game information that prepare_df() would join from the schedule
    spread_line = 3,
    total_line = 45,
    posteam_total = 24,
    roof = "dome",
    fg_model_roof = paste0("0", get_fg_era(season)),
    era3 = 0,
    era4 = 1,
    outdoors = 0,
    retractable = 0,
    dome = 1,
    runoff = 0
  ) |>
    dplyr::mutate(
      down = 4,
      half_seconds_remaining = quarter_seconds_remaining,
      game_seconds_remaining = quarter_seconds_remaining,
      home_timeouts_remaining = 2,
      away_timeouts_remaining = 2,
      home_receive_2h_ko = 0,
      original_posteam = posteam,
      posteam_spread = spread_line
    ) |>
    prepare_ot()
}
