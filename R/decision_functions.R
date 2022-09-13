
# #########################################################################################
# now the main functions that calculate WP for a given decision

# punts
get_punt_wp <- function(pbp) {

  # to join later
  pbp <- pbp %>% mutate(punt_index = 1 : n ())

  # get wp associated with punt
  probs <- pbp %>%
    left_join(punt_df, by = "yardline_100") %>%
    flip_team() %>%
    mutate(
      yardline_100 = 100 - yardline_after,

      # deal with punt return TD (yardline_after == 100) or muff (muff == 1)
      # we want punting team to be receiving a kickoff so have to flip everything back
      posteam = case_when(
        (yardline_after == 100 | muff == 1) & original_posteam == away_team ~ away_team,
        (yardline_after == 100 | muff == 1) & original_posteam == home_team ~ home_team,
        TRUE ~ posteam
      ),

      posteam_timeouts_remaining = case_when(
        (yardline_after == 100 | muff == 1) & original_posteam == home_team ~ home_timeouts_remaining,
        (yardline_after == 100 | muff == 1) & original_posteam == away_team ~ away_timeouts_remaining,
        TRUE ~ posteam_timeouts_remaining
      ),

      defteam_timeouts_remaining = case_when(
        (yardline_after == 100 | muff == 1) & original_posteam == home_team ~ away_timeouts_remaining,
        (yardline_after == 100 | muff == 1) & original_posteam == away_team ~ home_timeouts_remaining,
        TRUE ~ defteam_timeouts_remaining
      ),

      # return TD and muff for yardline
      yardline_100 = if_else(yardline_after == 100, as.integer(75), as.integer(yardline_100)),
      yardline_100 = if_else(muff == 1, as.integer(100 - yardline_100), yardline_100),

      score_differential = if_else(yardline_after == 100, as.integer(-score_differential - 7), as.integer(score_differential)),
      score_differential = if_else(muff == 1, as.integer(-score_differential), as.integer(score_differential)),

      receive_2h_ko = case_when(
        qtr <= 2 & receive_2h_ko == 0 & (yardline_after == 100 | muff == 1) ~ 1,
        qtr <= 2 & receive_2h_ko == 1 & (yardline_after == 100 | muff == 1) ~ 0,
        TRUE ~ receive_2h_ko
      ),

      ydstogo = if_else(yardline_100 < 10, yardline_100, as.integer(ydstogo))

    ) %>%
    flip_half() %>%
    nflfastR::calculate_win_probability() %>%
    mutate(
      vegas_wp = if_else(posteam != original_posteam, 1 - vegas_wp, vegas_wp)
    ) %>%
    end_game_fn() %>%
    mutate(
      wt_wp = pct * vegas_wp
    ) %>%
    group_by(punt_index) %>%
    summarize(punt_wp = sum(wt_wp)) %>%
    ungroup()

  pbp %>%
    left_join(probs, by = "punt_index") %>%
    select(-punt_index)

}

# field goals
get_fg_wp <- function(pbp) {

  # probability field goal is made
  fg_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = pbp, type="response")) %>%
    as_tibble() %>%
    dplyr::rename(fg_make_prob = value)

  dat <- bind_cols(
    pbp,
    fg_prob
  ) %>%
    mutate(
      # don't recommend kicking when fg is over 62 yards (this is very scientific)
      fg_make_prob = ifelse(yardline_100 > 44, 0, fg_make_prob),
      # hacky way to not have crazy high probs for long kicks
      # because the bot should be conservative about recommending kicks in this region
      # for 56 through 60 yards

      # note: if you're implementing this for your own team, provide your own estimates of your kicker's
      # true probs
      fg_make_prob = ifelse(yardline_100 >= 38, .9 * fg_make_prob, fg_make_prob),

      fg_index = 1 : n()
    )

  make_df <- dat %>%
    flip_team() %>%
    # win prob after receiving kickoff for touchback and other team has 3 more points
    mutate(
      yardline_100 = 75,
      score_differential = score_differential - 3
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    nflfastR::calculate_win_probability() %>%
    mutate(
      vegas_wp = if_else(posteam != original_posteam, 1 - vegas_wp, vegas_wp)
    ) %>%
    end_game_fn() %>%
    select(fg_index, make_fg_wp = vegas_wp)

  miss_df <- dat %>%
    flip_team() %>%
    mutate(
      yardline_100 = (100 - yardline_100) - 8,
      # yardline_100 can't be bigger than 80 due to some weird nfl rule
      yardline_100 = if_else(yardline_100 > 80, 80, yardline_100),
      yardline_100 = ifelse(yardline_100 < 1, 1, yardline_100)
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    nflfastR::calculate_win_probability() %>%
    mutate(
      vegas_wp = if_else(posteam != original_posteam, 1 - vegas_wp, vegas_wp)
    ) %>%
    end_game_fn() %>%
    select(fg_index, miss_fg_wp = vegas_wp)

  dat %>%
    left_join(make_df, by = "fg_index") %>%
    left_join(miss_df, by = "fg_index") %>%
    mutate(fg_wp = fg_make_prob * make_fg_wp + (1 - fg_make_prob) * miss_fg_wp) %>%
    select(-fg_index) %>%
    return()
}

# function to get WPs for go for 1 or go for 2
# this is here because it's needed for the going for 4th down model
get_2pt_wp <- function(pbp) {

  pbp <- pbp %>% mutate(index_2pt = 1 : n())

  # stuff in the 2pt model
  data <- pbp %>%
    mutate(era2 = 0) %>%
    select(
      era2,  era3,     era4,     outdoors,
      retractable,  dome,    posteam_spread, total_line,  posteam_total
    )

  # get probability of converting 2pt attempt from model
  prob_2pt <- stats::predict(
    two_pt_model,
    as.matrix(data)
  )  %>%
    tibble::as_tibble() %>%
    dplyr::rename(prob_2pt = "value") %>%
    select(prob_2pt)

  # probability of making PAT
  xp_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = pbp %>% mutate(yardline_100 = 15), type="response")) %>%
    as_tibble() %>%
    dplyr::rename(prob_1pt = "value") %>%
    select(prob_1pt)

  pbp <- bind_cols(
    pbp, prob_2pt, xp_prob
  )

  probs <- bind_rows(
    pbp %>% mutate(pts = 0, score_differential = -score_differential),
    pbp %>% mutate(pts = 1, score_differential = -score_differential - 1),
    pbp %>% mutate(pts = 2, score_differential = -score_differential - 2)
  ) %>%
    mutate(
      # switch posteam, timeouts and kickoff indicator
      posteam = case_when(
        home_team == posteam ~ away_team,
        away_team == posteam ~ home_team
      ),

      posteam_timeouts_remaining = ifelse(original_posteam == home_team, away_timeouts_remaining, home_timeouts_remaining),
      defteam_timeouts_remaining = ifelse(original_posteam == home_team, home_timeouts_remaining, away_timeouts_remaining),

      receive_2h_ko = case_when(
        qtr <= 2 & receive_2h_ko == 0 ~ 1,
        qtr <= 2 & receive_2h_ko == 1 ~ 0,
        TRUE ~ receive_2h_ko
      ),

      # 1st & 10 after touchback
      yardline_100 = 75,
      down = 1,
      ydstogo = 10

    ) %>%
    flip_half() %>%
    nflfastR::calculate_win_probability() %>%
    mutate(vegas_wp = if_else(posteam != original_posteam, 1 - vegas_wp, vegas_wp)) %>%
    arrange(index_2pt, pts) %>%
    select(index_2pt, pts, vegas_wp, prob_2pt, prob_1pt) %>%
    group_by(index_2pt) %>%
    summarize(
      wp_0 = dplyr::first(vegas_wp),
      wp_1 = dplyr::nth(vegas_wp, 2),
      wp_2 = dplyr::last(vegas_wp),
      conv_2pt = dplyr::first(prob_2pt),
      conv_1pt = dplyr::first(prob_1pt)
    ) %>%
    mutate(
      wp_go2 = conv_2pt * wp_2 + (1 - conv_2pt) * wp_0,
      wp_go1 = conv_1pt * wp_1 + (1 - conv_1pt) * wp_0,

      # convenience column useful for other things
      wp_td = ifelse(wp_go1 > wp_go2, wp_go1, wp_go2)
    ) %>%
    ungroup()

  pbp %>%
    left_join(probs, by = "index_2pt") %>%
    select(-index_2pt) %>%
    return()

}

# go for it on 4th down WP
get_go_wp <- function(pbp) {

  n_plays <- nrow(pbp)
  pbp <- pbp %>% mutate(go_index = 1 : n())

  # stuff in the go for it model
  data <- pbp %>%
    select(
      down,    ydstogo,     yardline_100,  era3,     era4,     outdoors,
      retractable,  dome,    posteam_spread, total_line,  posteam_total
    )

  # get model output from situation
  preds_df <- stats::predict(
    fd_model,
    as.matrix(data)
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(prob = "value") %>%
    dplyr::bind_cols(
      tibble::tibble(
        "gain" = rep_len(-10:65, length.out = n_plays * 76),
        "go_index" = rep(pbp$go_index, times = rep_len(76, length.out = n_plays))
      ) %>%
        dplyr::left_join(pbp, by = "go_index")
    ) %>%
    dplyr::mutate(
      # if predicted gain is more than possible, call it a TD
      gain = ifelse(gain > yardline_100, as.integer(yardline_100), as.integer(gain))
    ) %>%

    # this step is to combine all the TD probs into one (for gains longer than possible)
    group_by(go_index, gain) %>%
    mutate(prob = sum(prob)) %>%
    dplyr::slice(1) %>%

    # needed for the max() step later
    group_by(go_index) %>%

    # update situation based on play result
    mutate(
      yardline_100 = yardline_100 - gain,
      # for figuring out if it was a td later
      final_yardline = yardline_100,
      turnover = ifelse(gain < ydstogo, 1, 0),
      down = 1,

      # flip a bunch of columns on turnover on downs where other team gets ball
      # # note: touchdowns are dealt with separately later
      yardline_100 = ifelse(turnover == 1, 100 - yardline_100, yardline_100),

      posteam_timeouts_remaining = case_when(
        turnover == 1 & original_posteam == home_team ~ away_timeouts_remaining,
        turnover == 1 & original_posteam == away_team ~ home_timeouts_remaining,
        TRUE ~ posteam_timeouts_remaining
      ),

      defteam_timeouts_remaining = case_when(
        turnover == 1 & original_posteam == home_team ~ home_timeouts_remaining,
        turnover == 1 & original_posteam == away_team ~ away_timeouts_remaining,
        TRUE ~ defteam_timeouts_remaining
      ),

      # flip receive_2h_ko var if turnover
      receive_2h_ko = case_when(
        qtr <= 2 & receive_2h_ko == 0 & turnover == 1 ~ 1,
        qtr <= 2 & receive_2h_ko == 1 & turnover == 1 ~ 0,
        TRUE ~ receive_2h_ko
      ),

      # switch posteam if turnover
      posteam = case_when(
        home_team == posteam & turnover == 1 ~ away_team,
        away_team == posteam & turnover == 1 ~ home_team,
        TRUE ~ posteam
      ),

      # swap score diff if turnover on downs
      score_differential = ifelse(turnover == 1, -score_differential, score_differential),

      # give 6 points for the TD plays
      score_differential = ifelse(yardline_100 == 0, score_differential + 6, score_differential),

      # run off 6 seconds
      half_seconds_remaining = half_seconds_remaining - 6,
      game_seconds_remaining = game_seconds_remaining - 6,

      # additional runoff after successful non-td conversion (entered from user input)
      half_seconds_remaining = ifelse(turnover == 0 & yardline_100 > 0, half_seconds_remaining - runoff, half_seconds_remaining),
      game_seconds_remaining = ifelse(turnover == 0 & yardline_100 > 0, game_seconds_remaining - runoff, game_seconds_remaining),

      # after all that, make sure these aren't negative
      half_seconds_remaining = max(half_seconds_remaining, 0),
      game_seconds_remaining = max(game_seconds_remaining, 0),

      # if now goal to go for either team, use yardline for yards to go, otherwise it's 1st and 10
      ydstogo = ifelse(yardline_100 < 10, yardline_100, 10)

    ) %>%
    ungroup()

  # separate df of just the TDs to calculate WP after TD
  # this step is needed to deal with the option of 1pt or 2pt choice
  if (nrow(preds_df %>% filter(yardline_100 == 0)) > 0) {
    tds_df <- preds_df %>%
      filter(yardline_100 == 0) %>%
      get_2pt_wp() %>%
      select(go_index, yardline_100, wp_td)
  } else {
    # avoids errors when one play is fed that doesn't have TD in range
    tds_df <- tibble::tibble(
      "go_index" = 0,
      "yardline_100" = 99999
    )
  }


  # join TD WPs back to original df and use those WPs
  preds <- preds_df %>%
    left_join(tds_df, by = c("go_index", "yardline_100")) %>%
    flip_half() %>%
    nflfastR::calculate_win_probability() %>%
    mutate(
      # flip WP for possession change (turnover)
      vegas_wp = if_else(posteam != original_posteam, 1 - vegas_wp, vegas_wp),

      # get the TD probs computed separately
      vegas_wp = ifelse(yardline_100 == 0, wp_td, vegas_wp),

      # fill in end of game situation when team can kneel out clock after successful non-td conversion
      vegas_wp = case_when(
        score_differential > 0 & turnover == 0 & yardline_100 > 0 & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 1,
        score_differential > 0 & turnover == 0 & yardline_100 > 0 & game_seconds_remaining < 80 & defteam_timeouts_remaining == 1 ~ 1,
        score_differential > 0 & turnover == 0 & yardline_100 > 0 & game_seconds_remaining < 40 & defteam_timeouts_remaining == 2 ~ 1,
        TRUE ~ vegas_wp
      ),
      # fill in end of game situation when other team can kneel out clock after failed attempt
      vegas_wp = case_when(
        score_differential > 0 & turnover == 1 & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 0,
        score_differential > 0 & turnover == 1 & game_seconds_remaining < 80 & defteam_timeouts_remaining == 1 ~ 0,
        score_differential > 0 & turnover == 1 & game_seconds_remaining < 40 & defteam_timeouts_remaining == 2 ~ 0,
        TRUE ~ vegas_wp
      ),

      wt_wp = prob * vegas_wp
    )

  report <- preds %>%
    group_by(go_index, turnover) %>%
    mutate(
      fd_pct = sum(prob),
      new_prob = prob / fd_pct,
      wt_wp = new_prob * vegas_wp
    ) %>%
    summarize(
      pct = sum(prob),
      wp = sum(wt_wp)
    ) %>%
    pivot_wider(
      names_from = turnover, values_from = c("pct", "wp")
    ) %>%
    dplyr::rename(
      first_down_prob = pct_0,
      wp_fail = wp_1,
      wp_succeed = wp_0
    ) %>%
    ungroup() %>%
    dplyr::select(go_index, first_down_prob, wp_fail, wp_succeed)

  wp_go_df <- preds %>%
    group_by(go_index) %>%
    summarize(go_wp = sum(wt_wp)) %>%
    ungroup()

  pbp %>%
    left_join(report, by = "go_index") %>%
    left_join(wp_go_df, by = "go_index") %>%
    select(-go_index) %>%
    return()

}
