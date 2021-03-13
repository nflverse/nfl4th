# # # # a couple basic helpers

# for standardizing team names
team_name_fn <- function(var) {
  stringr::str_replace_all(
    var,
    c(
      "JAC" = "JAX",
      "STL" = "LA",
      "SL" = "LA",
      "ARZ" = "ARI",
      "BLT" = "BAL",
      "CLV" = "CLE",
      "HST" = "HOU",
      "SD" = "LAC",
      "OAK" = "LV"
    )
  )
}

# helper column to avoid join errors
drop.cols <- c(
  "game_id", "week",  "model_roof", "era3", "era4", "era", "home_total", "away_total", "total_line", "spread_line",
  "retractable", "dome", "outdoors"
)

# loading and cleaning games file
get_games_file <- function() {

  rds_from_url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true") %>%
    filter(season > 2013) %>%
    mutate(
      type = if_else(game_type == "REG", "reg", "post"),
      model_roof = if_else(roof == "open" | roof == "closed" | is.na(roof), "retractable", roof),
      # don't use this for games earlier than 2014
      era3 = dplyr::if_else(season > 2013 & season <= 2017, 1, 0),
      era4 = dplyr::if_else(season > 2017, 1, 0),
      # for field goal model
      era = 3,
      home_total = (total_line + spread_line) / 2,
      away_total = (total_line - spread_line) / 2,
      retractable = dplyr::if_else(model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(model_roof == 'outdoors', 1, 0)
    ) %>%
    dplyr::mutate_at(dplyr::vars("home_team", "away_team"), team_name_fn) %>%
    dplyr::select(
      game_id, season, type, week, away_team, home_team, espn,
      model_roof, era3, era4, era, home_total, away_total, total_line, spread_line,
      retractable, dome, outdoors
    ) %>%
    return()
}

# data prep function
prepare_df <- function(df) {

  df %>%
    # if an nflfastR df is passed, need to drop all this so we don't get redundant cols
    dplyr::select(-tidyselect::any_of(drop.cols)) %>%
    left_join(.games_nfl4th, by = c("home_team", "away_team", "type", "season")) %>%
    mutate(
      receive_2h_ko = case_when(
        # 1st half, home team opened game with kickoff, away team has ball
        qtr <= 2 & home_opening_kickoff == 1 & posteam == away_team ~ 1,
        # 1st half, away team opened game with kickoff, home team has ball
        qtr <= 2 & home_opening_kickoff == 0 & posteam == home_team ~ 1,
        TRUE ~ 0
      ),
      down = 4,
      half_seconds_remaining = if_else(qtr == 2 | qtr == 4, quarter_seconds_remaining, quarter_seconds_remaining + 900),
      game_seconds_remaining = if_else(qtr <= 2, half_seconds_remaining + 1800, half_seconds_remaining),
      posteam_spread = if_else(posteam == home_team, spread_line, -spread_line),
      posteam_total = if_else(posteam == home_team, home_total, away_total),
      posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line),

      # useful for lots of stuff later

      home_timeouts_remaining = if_else(posteam == home_team, posteam_timeouts_remaining, defteam_timeouts_remaining),
      away_timeouts_remaining = if_else(posteam == away_team, posteam_timeouts_remaining, defteam_timeouts_remaining),
      original_posteam = posteam
    ) %>%
    return()

}

# helper function for switching possession and running off 6 seconds
flip_team <- function(df) {

  df %>%
    mutate(
      # switch posteam
      posteam = if_else(home_team == posteam, away_team, home_team),

      # update timeouts
      posteam_timeouts_remaining = if_else(posteam == away_team, away_timeouts_remaining, home_timeouts_remaining),
      defteam_timeouts_remaining = if_else(posteam == home_team, away_timeouts_remaining, home_timeouts_remaining),
      # swap score
      score_differential = -score_differential,
      # 1st and 10
      down = 1,
      ydstogo = 10,
      # run off 6 seconds
      half_seconds_remaining = half_seconds_remaining - 6,
      game_seconds_remaining = game_seconds_remaining - 6,
      # don't let seconds go negative
      half_seconds_remaining = if_else(half_seconds_remaining < 0, 0, half_seconds_remaining),
      game_seconds_remaining = if_else(game_seconds_remaining < 0, 0, game_seconds_remaining),
      # flip receive_2h_ko var
      receive_2h_ko = case_when(
        qtr <= 2 & receive_2h_ko == 0 ~ 1,
        qtr <= 2 & receive_2h_ko == 1 ~ 0,
        TRUE ~ receive_2h_ko
      )
    ) %>%
    return()

}

# helper function to move the game to start of 3rd Q on an end-of-half play
# on the plays where we find that the half has ended
flip_half <- function(df) {

  df %>%
    mutate(
      prior_posteam = posteam,
      end_of_half = ifelse(
        qtr == 2 & half_seconds_remaining == 0, 1, 0
      ),
      posteam = case_when(
        home_opening_kickoff == 1 & end_of_half == 1 ~ away_team,
        home_opening_kickoff == 0 & end_of_half == 1 ~ home_team,
        TRUE ~ posteam
      ),
      qtr = ifelse(end_of_half == 1, 3L, qtr),
      posteam_timeouts_remaining = ifelse(end_of_half == 1, 3L, posteam_timeouts_remaining),
      defteam_timeouts_remaining = ifelse(end_of_half == 1, 3L, defteam_timeouts_remaining),
      down = ifelse(end_of_half == 1, 1, down),
      ydstogo = ifelse(end_of_half == 1, 10L, ydstogo),
      yardline_100 = ifelse(end_of_half == 1, 75L, yardline_100),
      half_seconds_remaining = ifelse(end_of_half == 1, 1800, half_seconds_remaining),
      game_seconds_remaining = ifelse(end_of_half == 1, 1800, game_seconds_remaining),
      score_differential = ifelse(
        posteam != prior_posteam & end_of_half == 1, -score_differential, score_differential
      ),
      receive_2h_ko = ifelse(end_of_half == 1, 0, receive_2h_ko)
    ) %>%
    select(-prior_posteam, -end_of_half) %>%
    return()

}

# fill in end of game situation when team can kneel out clock
# discourages punting or fg when the other team can end the game
end_game_fn <- function(pbp) {

  pbp %>%
    mutate(
      vegas_wp = case_when(
        score_differential > 0 & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 0,
        score_differential > 0 & game_seconds_remaining < 80 & defteam_timeouts_remaining == 1 ~ 0,
        score_differential > 0 & game_seconds_remaining < 40 & defteam_timeouts_remaining == 2 ~ 0,
        TRUE ~ vegas_wp
      )
    ) %>%
    return()
}



# #########################################################################################
# other: this isn't used by the bot or shiny app but helps get an nflfastR df read

# prepare raw pbp data
prepare_nflfastr_data <- function(pbp) {

  # some prep
  data <- pbp %>%
    dplyr::mutate(
      type = ifelse(tolower(season_type) == "reg", "reg", "post")
    ) %>%
    filter(
      game_seconds_remaining > 30,
      !is.na(half_seconds_remaining),
      !is.na(qtr),
      !is.na(posteam),
      qtr <= 4
      )

  return(data)

}

# convenience function to add the probs from each model
add_probs <- function(df) {

  df %>%
    get_go_wp() %>%
    get_fg_wp() %>%
    get_punt_wp() %>%
    return()

}

rds_from_url <- function(.url){
  con <- url(.url)
  dat <- readRDS(con)
  close(con)
  dat
}

load_fd_model <- function() {
  fd_model <- NULL
  con <- url("https://github.com/guga31bb/fourth_calculator/blob/main/data/fd_model.Rdata?raw=true")
  try(load(con), silent = TRUE)
  close(con)
  fd_model
}
