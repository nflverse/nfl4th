
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

drop.me <- c("vegas_wp", "ep")

# apply the actual probabilities
calculate_win_probability <- function(pbp_data) {

  # drop existing values of ep and the probs before making new ones
  pbp_data <- pbp_data %>% dplyr::select(-tidyselect::any_of(drop.me))
  pbp_data <- pbp_data %>% dplyr::select(-ends_with("_prob"))

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
    dplyr::rename(vegas_wp = "value")

  preds <- dplyr::bind_cols(
    pbp_data,
    wp
  ) %>%
    # wp model estimates home wp. flip back for away teams
    mutate(vegas_wp = ifelse(original_posteam == away_team, 1 - vegas_wp, vegas_wp))

  return(preds)
}

