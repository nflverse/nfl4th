
# helper 1
#get the columns needed for wp predictions
#making sure they're in the right order
wp_model_select <- function(pbp) {

  pbp <- pbp %>%
    dplyr::select(
      "home_receive_2h_ko",
      "spread_time",
      "home_posteam",
      "half_seconds_remaining",
      "game_seconds_remaining",
      "Diff_Time_Ratio",
      "home_score_differential",
      "home_ep",
      "ydstogo",
      "home_yardline_100",
      "home_timeouts_remaining",
      "home_timeouts_remaining"
    )

  return(pbp)

}

# helper 2
# apply the predictions
get_preds_wp <- function(pbp) {

  preds <- stats::predict(wp_model, as.matrix(pbp %>% wp_model_select()))

  return(preds)
}

drop.me <- c("vegas_wp", "vegas_home_wp", "ep")

# apply the actual probabilities
calculate_win_probability <- function(pbp_data) {

  # drop existing values of ep and the probs before making new ones
  pbp_data <- pbp_data %>% dplyr::select(-tidyselect::any_of(drop.me))
  pbp_data <- pbp_data %>% dplyr::select(-ends_with("_prob"))

  # model 1: estimate home win probability
    model_data <- pbp_data %>%
      nflfastR::calculate_expected_points() %>%
      dplyr::mutate(
        home_score_differential = ifelse(posteam == home_team, score_differential, -score_differential),
        home_posteam = ifelse(home_team == posteam, 1, 0),
        home_yardline_100 = ifelse(posteam == home_team, yardline_100, 100 - yardline_100),
        home_ep = ifelse(posteam == home_team, ep, -ep),
        Diff_Time_Ratio = .data$home_score_differential / (exp(-4 * .data$elapsed_share))
      )

    wp <- get_preds_wp(model_data) %>%
      tibble::as_tibble() %>%
      dplyr::rename(vegas_home_wp = "value")

  # model 2: nflfastR possession team WP
    model_data <- pbp_data %>%
      dplyr::mutate(
        receive_2h_ko = case_when(
          # 1st half, home team opened game with kickoff, away team has ball
          qtr <= 2 & home_opening_kickoff == 1 & posteam == away_team ~ 1,
          # 1st half, away team opened game with kickoff, home team has ball
          qtr <= 2 & home_opening_kickoff == 0 & posteam == home_team ~ 1,
          TRUE ~ 0
        ),
        posteam_timeouts_remaining = if_else(posteam == away_team, away_timeouts_remaining, home_timeouts_remaining),
        defteam_timeouts_remaining = if_else(posteam == home_team, away_timeouts_remaining, home_timeouts_remaining)
      )

    wp2 <- nflfastR::calculate_win_probability(model_data) %>%
      tibble::as_tibble() %>%
      select(vegas_wp)

  preds <- dplyr::bind_cols(
    pbp_data,
    wp,
    wp2
  ) %>%
    mutate(
      # wp model estimates home wp. flip back for away teams. WP is from perspective of team with 4th down decision (original posteam)
      vegas_home_wp = ifelse(original_posteam == away_team, 1 - vegas_home_wp, vegas_home_wp),
      # nflfastr wp model uses possession team. flip back to original possession team
      vegas_wp = ifelse(posteam != original_posteam, 1 - vegas_wp, vegas_wp),
      # take the average
      vegas_wp = (vegas_home_wp + vegas_wp) / 2
      ) %>%
    select(-vegas_home_wp)

  return(preds)
}

