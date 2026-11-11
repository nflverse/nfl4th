# ##############################################################################
# now the main functions that calculate WP for a given decision

# punts
get_punt_wp <- function(pbp) {
  # to join later
  pbp <- pbp |> mutate(punt_index = 1:n())

  # get wp associated with punt: punt_df (data-raw/_punt_model.R) has the
  # probability of each outcome by line of scrimmage. Spots at the opponent's
  # 30 or closer aren't in it and get no punt WP
  probs <- pbp |>
    inner_join(punt_df, by = "yardline_100") |>
    mutate(.ot_possession = ot_possession) |>
    flip_team() |>
    mutate(
      # overtime: a muffed punt recovered by the punting team keeps its
      # possession going, a return touchdown ends the game
      ot_possession = dplyr::if_else(
        muff %in% 1,
        .ot_possession,
        ot_possession
      ),
      ot_end = dplyr::case_when(
        qtr > 4 & yardline_after %in% 100 ~ 0,
        muff %in% 1 ~ NA_real_,
        TRUE ~ ot_end
      ),

      yardline_100 = 100 - yardline_after,

      # deal with punt return TD (yardline_after == 100) or muff (muff == 1)
      # we want punting team to be receiving a kickoff so have to flip
      # everything back
      posteam = case_when(
        (yardline_after == 100 | muff == 1) &
          original_posteam == away_team ~ away_team,
        (yardline_after == 100 | muff == 1) &
          original_posteam == home_team ~ home_team,
        TRUE ~ posteam
      ),

      # muff / return TD stuff
      yardline_100 = ifelse(
        muff == 1,
        as.integer(100 - yardline_100),
        yardline_100
      ),
      yardline_100 = ifelse(
        yardline_after == 100,
        kickoff_yardline(season),
        as.integer(yardline_100)
      ),

      # muff / return TD stuff
      score_differential = if_else(
        yardline_after == 100,
        as.integer(-score_differential - 7),
        as.integer(score_differential)
      ),
      score_differential = if_else(
        muff == 1,
        as.integer(-score_differential),
        as.integer(score_differential)
      ),

      ydstogo = ifelse(yardline_100 < 10, yardline_100, as.integer(ydstogo))
    ) |>
    select(-".ot_possession") |>
    flip_half() |>
    calculate_win_probability() |>
    end_game_fn() |>
    mutate(
      wt_wp = pct * vegas_wp
    ) |>
    group_by(punt_index) |>
    summarize(punt_wp = sum(wt_wp)) |>
    ungroup()

  pbp |>
    left_join(probs, by = "punt_index") |>
    select(-punt_index)
}

# field goals
get_fg_wp <- function(pbp) {
  # Make probability is a lookup rather than a call to fg_model. The table is
  # built in data-raw/_punt_and_fg_models.R and already accounts for the fact
  # that only teams who trust their kicker attempt long field goals, so the
  # raw model curve is too generous past about 52 yards. It also fades to zero
  # from 65 to 75 yards. There is therefore no decay to apply here.
  fg_prob <- pbp |>
    dplyr::transmute(
      yardline_100 = as.integer(yardline_100),
      fg_model_roof = as.character(fg_model_roof)
    ) |>
    dplyr::left_join(
      fg_prob_df |> dplyr::mutate(fg_model_roof = as.character(fg_model_roof)),
      by = c("yardline_100", "fg_model_roof")
    ) |>
    dplyr::select(fg_make_prob)

  dat <- bind_cols(
    pbp,
    fg_prob
  ) |>
    mutate(
      fg_index = 1:n()
    )

  # win prob after receiving kickoff for touchback and other team has 3 more
  # points
  make_df <- dat |>

    # win prob after receiving kickoff for touchback and other team has 3 more
    # points. In overtime the rules decide whether the game goes on
    ot_field_goal() |>
    flip_team(ot_stop = FALSE) |>
    mutate(
      yardline_100 = kickoff_yardline(season),
      score_differential = score_differential - 3
    ) |>

    # for end of 1st half stuff
    flip_half() |>
    calculate_win_probability() |>
    end_game_fn() |>
    select(fg_index, make_fg_wp = vegas_wp)

  # win prob after missed FG
  miss_df <- dat |>
    flip_team() |>
    mutate(
      yardline_100 = (100 - yardline_100) - 8,
      # yardline_100 can't be bigger than 80 due to some weird nfl rule
      yardline_100 = if_else(yardline_100 > 80, 80, yardline_100),
      yardline_100 = ifelse(yardline_100 < 1, 1, yardline_100)
    ) |>
    # for end of 1st half stuff
    flip_half() |>
    calculate_win_probability() |>
    end_game_fn() |>
    select(fg_index, miss_fg_wp = vegas_wp)

  dat |>
    left_join(make_df, by = "fg_index") |>
    left_join(miss_df, by = "fg_index") |>
    mutate(
      fg_wp = fg_make_prob * make_fg_wp + (1 - fg_make_prob) * miss_fg_wp
    ) |>
    select(-fg_index)
}

# function to get WPs for go for 1 or go for 2
# A 2 point try is a 4th and goal from the 2, so its conversion probability
# comes from the go for it model, with the game's spread and total. Unlike 4th
# downs, 2 point tries convert about as often in every era (48.1% in 2001-2019,
# 49.1% since 2020), so every season uses the go for it model's latest era (see
# get_fd_era()). The go for it model never sees a 2 point try. On 2001-2026
# tries from the 2 it predicts 48.9% against 48.5% actual, with a lower log
# loss than a constant (0.6902 vs 0.6927). On 2020-2026 tries its log loss is
# 0.6916, against 0.6955 for the xgboost model this replaces (trained on
# 2010-2019).
two_pt_conversion_prob <- function(pbp) {
  try_2pt <- data.frame(
    yardline_100 = rep(2L, nrow(pbp)),
    ydstogo = rep(2L, nrow(pbp)),
    season = pmax(pbp$season, 2020L),
    posteam_spread = pbp$posteam_spread,
    total_line = pbp$total_line
  )
  gains <- fd_gain_probs(try_2pt)
  as.numeric(tapply(
    gains$prob * (gains$gain >= 2),
    factor(gains$row, levels = seq_len(nrow(try_2pt))),
    sum
  ))
}

# this is here because it's needed for the going for 4th down model
get_2pt_wp <- function(pbp) {
  pbp <- pbp |> mutate(index_2pt = 1:n())

  prob_2pt <- tibble::tibble(prob_2pt = two_pt_conversion_prob(pbp))

  # probability of making PAT
  xp_prob <- as.numeric(mgcv::predict.bam(
    fg_model,
    newdata = pbp |> mutate(yardline_100 = 15),
    type = "response"
  )) |>
    as_tibble() |>
    dplyr::rename(prob_1pt = "value") |>
    select(prob_1pt)

  pbp <- bind_cols(
    pbp,
    prob_2pt,
    xp_prob
  )

  # in overtime the rules decide whether the game goes on after the try
  probs <- bind_rows(
    pbp |>
      ot_touchdown(0) |>
      mutate(pts = 0, score_differential = -score_differential),
    pbp |>
      ot_touchdown(1) |>
      mutate(pts = 1, score_differential = -score_differential - 1),
    pbp |>
      ot_touchdown(2) |>
      mutate(pts = 2, score_differential = -score_differential - 2)
  ) |>
    mutate(
      # switch posteam
      posteam = case_when(
        home_team == posteam ~ away_team,
        away_team == posteam ~ home_team
      ),

      # 1st & 10 after the kickoff
      yardline_100 = kickoff_yardline(season),
      down = 1,
      ydstogo = 10
    ) |>
    flip_half() |>
    calculate_win_probability() |>
    arrange(index_2pt, pts) |>
    select(index_2pt, pts, vegas_wp, prob_2pt, prob_1pt) |>
    group_by(index_2pt) |>
    summarize(
      wp_0 = dplyr::first(vegas_wp),
      wp_1 = dplyr::nth(vegas_wp, 2),
      wp_2 = dplyr::last(vegas_wp),
      conv_2pt = dplyr::first(prob_2pt),
      conv_1pt = dplyr::first(prob_1pt)
    ) |>
    mutate(
      wp_go2 = conv_2pt * wp_2 + (1 - conv_2pt) * wp_0,
      wp_go1 = conv_1pt * wp_1 + (1 - conv_1pt) * wp_0,

      # convenience column useful for other things
      wp_td = ifelse(wp_go1 > wp_go2, wp_go1, wp_go2)
    ) |>
    ungroup()

  pbp |>
    left_join(probs, by = "index_2pt") |>
    select(-index_2pt)
}

# go for it on 4th down WP
get_go_wp <- function(pbp) {
  pbp <- pbp |> mutate(go_index = 1:n())

  # distribution of yards gained when going for it (see R/go_model.R), which
  # converts more often in late and desperate situations. A gain equal to
  # yardline_100 is a touchdown
  gains <- fd_gain_probs(pbp, late = fd_late(pbp))
  preds_df <- tibble::tibble(
    go_index = pbp$go_index[gains$row],
    gain = as.integer(gains$gain),
    prob = gains$prob
  ) |>
    dplyr::left_join(pbp, by = "go_index") |>

    # needed for the max() step later
    group_by(go_index) |>

    # update situation based on play result
    mutate(
      yardline_100 = yardline_100 - gain,
      # for figuring out if it was a td later
      final_yardline = yardline_100,
      turnover = ifelse(gain < ydstogo, 1, 0),
      down = 1,

      # overtime: a team that trails in the second possession loses when it
      # turns the ball over, otherwise the other team's possession starts
      ot_end = dplyr::if_else(
        turnover == 1 &
          qtr > 4 &
          ot_rule != "SD" &
          ot_possession == 2 &
          score_differential < 0,
        0,
        ot_end
      ),
      ot_possession = dplyr::case_when(
        turnover == 0 | qtr <= 4 ~ ot_possession,
        ot_rule == "SD" ~ 3,
        TRUE ~ pmin(ot_possession + 1, 3)
      ),

      # flip a bunch of columns on turnover on downs where other team gets ball
      # # note: touchdowns are dealt with separately later
      yardline_100 = ifelse(turnover == 1, 100 - yardline_100, yardline_100),

      # switch posteam if turnover
      posteam = case_when(
        home_team == posteam & turnover == 1 ~ away_team,
        away_team == posteam & turnover == 1 ~ home_team,
        TRUE ~ posteam
      ),

      # swap score diff if turnover on downs
      score_differential = ifelse(
        turnover == 1,
        -score_differential,
        score_differential
      ),

      # give 6 points for the TD plays
      score_differential = ifelse(
        yardline_100 == 0,
        score_differential + 6,
        score_differential
      ),

      # the clock of the next snap. A turnover on downs or a touchdown stops
      # the clock, so the next state comes 6 seconds (the play) later. After a
      # conversion that doesn't score the clock keeps running: the typical gap
      # to the offense's next snap (conversion_runoff()), or 6 seconds plus
      # `runoff` when it is supplied
      elapsed = dplyr::case_when(
        turnover == 1 | yardline_100 <= 0 ~ 6,
        is.na(runoff) ~ conversion_runoff(
          qtr,
          half_seconds_remaining,
          score_differential
        ),
        TRUE ~ 6 + runoff
      ),
      half_seconds_remaining = pmax(half_seconds_remaining - elapsed, 0),
      game_seconds_remaining = pmax(game_seconds_remaining - elapsed, 0),
      elapsed = NULL,

      # if now goal to go for either team, use yardline for yards to go,
      # otherwise it's 1st and 10
      ydstogo = ifelse(yardline_100 < 10, yardline_100, 10)
    ) |>
    ungroup()

  # separate df of just the TDs to calculate WP after TD
  # this step is needed to deal with the option of 1pt or 2pt choice
  if (nrow(preds_df |> filter(yardline_100 == 0)) > 0) {
    tds_df <- preds_df |>
      filter(yardline_100 == 0) |>
      get_2pt_wp() |>
      select(go_index, yardline_100, wp_td)
  } else {
    # avoids errors when one play is fed that doesn't have TD in range
    tds_df <- tibble::tibble(
      "go_index" = 0,
      "yardline_100" = 99999
    )
  }

  # join TD WPs back to original df and use those WPs
  preds <- preds_df |>
    left_join(tds_df, by = c("go_index", "yardline_100")) |>
    flip_half() |>
    calculate_win_probability() |>
    mutate(
      # get the TD probs computed separately
      vegas_wp = ifelse(yardline_100 == 0, wp_td, vegas_wp),

      # fill in end of game situation when team can kneel out clock after
      # successful non-td conversion
      defteam_timeouts_remaining = ifelse(
        posteam == home_team,
        away_timeouts_remaining,
        home_timeouts_remaining
      ),
      vegas_wp = case_when(
        qtr > 4 ~ vegas_wp,
        score_differential > 0 &
          turnover == 0 &
          yardline_100 > 0 &
          game_seconds_remaining < 120 &
          defteam_timeouts_remaining == 0 ~ 1,
        score_differential > 0 &
          turnover == 0 &
          yardline_100 > 0 &
          game_seconds_remaining < 80 &
          defteam_timeouts_remaining == 1 ~ 1,
        score_differential > 0 &
          turnover == 0 &
          yardline_100 > 0 &
          game_seconds_remaining < 40 &
          defteam_timeouts_remaining == 2 ~ 1,
        TRUE ~ vegas_wp
      ),
      # fill in end of game situation when other team can kneel out clock after
      # failed attempt
      vegas_wp = case_when(
        qtr > 4 ~ vegas_wp,
        score_differential > 0 &
          turnover == 1 &
          game_seconds_remaining < 120 &
          defteam_timeouts_remaining == 0 ~ 0,
        score_differential > 0 &
          turnover == 1 &
          game_seconds_remaining < 80 &
          defteam_timeouts_remaining == 1 ~ 0,
        score_differential > 0 &
          turnover == 1 &
          game_seconds_remaining < 40 &
          defteam_timeouts_remaining == 2 ~ 0,
        TRUE ~ vegas_wp
      ),

      wt_wp = prob * vegas_wp
    )

  report <- preds |>
    group_by(go_index, turnover) |>
    mutate(
      fd_pct = sum(prob),
      new_prob = prob / fd_pct,
      wt_wp = new_prob * vegas_wp
    ) |>
    summarize(
      pct = sum(prob),
      wp = sum(wt_wp)
    ) |>
    pivot_wider(
      names_from = turnover,
      values_from = c("pct", "wp")
    ) |>
    dplyr::rename(
      first_down_prob = pct_0,
      wp_fail = wp_1,
      wp_succeed = wp_0
    ) |>
    ungroup() |>
    dplyr::select(go_index, first_down_prob, wp_fail, wp_succeed)

  wp_go_df <- preds |>
    group_by(go_index) |>
    summarize(go_wp = sum(wt_wp)) |>
    ungroup()

  pbp |>
    left_join(report, by = "go_index") |>
    left_join(wp_go_df, by = "go_index") |>
    select(-go_index)
}
