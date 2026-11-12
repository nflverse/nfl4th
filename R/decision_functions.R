# ##############################################################################
# now the main functions that calculate WP for a given decision

# punts
get_punt_wp <- function(pbp) {
  # to join later
  pbp$punt_index <- seq_len(nrow(pbp))

  # get wp associated with punt: punt_df (data-raw/_punt_model.R) has the
  # probability of each outcome by line of scrimmage. Spots at the opponent's
  # 30 or closer aren't in it and get no punt WP. Every outcome of a play is a
  # row (an inner join on the line of scrimmage)
  key <- vctrs::vec_cast_common(pbp$yardline_100, punt_df$yardline_100)
  m <- vctrs::vec_locate_matches(key[[1]], key[[2]], no_match = "drop")
  probs <- pbp[m$needles, , drop = FALSE]
  probs$yardline_100 <- key[[1]][m$needles]
  for (n in setdiff(names(punt_df), "yardline_100")) {
    probs[[n]] <- punt_df[[n]][m$haystack]
  }
  probs$.ot_possession <- probs$ot_possession
  probs <- flip_team(probs)

  # overtime: a muffed punt recovered by the punting team keeps its
  # possession going, a return touchdown ends the game
  probs$ot_possession <- if_else(
    probs$muff %in% 1,
    probs$.ot_possession,
    probs$ot_possession
  )
  probs$ot_end <- case_when(
    probs$qtr > 4 & probs$yardline_after %in% 100 ~ 0,
    probs$muff %in% 1 ~ NA_real_,
    TRUE ~ probs$ot_end
  )
  probs$yardline_100 <- 100 - probs$yardline_after

  # deal with punt return TD (yardline_after == 100) or muff (muff == 1)
  # we want punting team to be receiving a kickoff so have to flip
  # everything back
  td_or_muff <- probs$yardline_after == 100 | probs$muff == 1
  probs$posteam <- case_when(
    td_or_muff & probs$original_posteam == probs$away_team ~ probs$away_team,
    td_or_muff & probs$original_posteam == probs$home_team ~ probs$home_team,
    TRUE ~ probs$posteam
  )

  # muff / return TD stuff
  probs$yardline_100 <- ifelse(
    probs$muff == 1,
    as.integer(100 - probs$yardline_100),
    probs$yardline_100
  )
  probs$yardline_100 <- ifelse(
    probs$yardline_after == 100,
    kickoff_yardline(probs$season),
    as.integer(probs$yardline_100)
  )
  probs$score_differential <- if_else(
    probs$yardline_after == 100,
    as.integer(-probs$score_differential - 7),
    as.integer(probs$score_differential)
  )
  probs$score_differential <- if_else(
    probs$muff == 1,
    as.integer(-probs$score_differential),
    as.integer(probs$score_differential)
  )
  probs$ydstogo <- ifelse(
    probs$yardline_100 < 10,
    probs$yardline_100,
    as.integer(probs$ydstogo)
  )
  probs$.ot_possession <- NULL

  probs <- end_game_fn(calculate_win_probability(flip_half(probs)))
  # the outcomes' WP weighted by their probability, summed by play in row
  # order (as summarize() does)
  wt_wp <- probs$pct * probs$vegas_wp
  s <- vapply(split(wt_wp, probs$punt_index), sum, numeric(1))
  pbp$punt_wp <- unname(s[match(pbp$punt_index, as.integer(names(s)))])
  pbp$punt_index <- NULL
  pbp
}

# field goals
get_fg_wp <- function(pbp) {
  # Make probability is a lookup rather than a call to fg_model. The table is
  # built in data-raw/_punt_and_fg_models.R and already accounts for the fact
  # that only teams who trust their kicker attempt long field goals, so the
  # raw model curve is too generous past about 52 yards. It also fades to zero
  # from 65 to 75 yards. There is therefore no decay to apply here.
  i <- vctrs::vec_match(
    data.frame(
      yardline_100 = as.integer(pbp$yardline_100),
      fg_model_roof = as.character(pbp$fg_model_roof)
    ),
    data.frame(
      yardline_100 = fg_prob_df$yardline_100,
      fg_model_roof = as.character(fg_prob_df$fg_model_roof)
    )
  )
  dat <- pbp
  dat$fg_make_prob <- fg_prob_df$fg_make_prob[i]

  # win prob after receiving kickoff for touchback and other team has 3 more
  # points. In overtime the rules decide whether the game goes on
  make <- flip_team(ot_field_goal(dat), ot_stop = FALSE)
  make$yardline_100 <- kickoff_yardline(make$season)
  make$score_differential <- make$score_differential - 3
  # (flip_half() for end of 1st half stuff)
  make <- end_game_fn(calculate_win_probability(flip_half(make)))

  # win prob after missed FG
  miss <- flip_team(dat)
  yl <- (100 - miss$yardline_100) - 8
  # yardline_100 can't be bigger than 80 due to some weird nfl rule
  yl <- if_else(yl > 80, 80, yl)
  miss$yardline_100 <- ifelse(yl < 1, 1, yl)
  miss <- end_game_fn(calculate_win_probability(flip_half(miss)))

  # both keep the rows of dat in order
  dat$make_fg_wp <- make$vegas_wp
  dat$miss_fg_wp <- miss$vegas_wp
  dat$fg_wp <- dat$fg_make_prob *
    dat$make_fg_wp +
    (1 - dat$fg_make_prob) * dat$miss_fg_wp
  dat
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
  n <- nrow(pbp)
  pbp$index_2pt <- seq_len(n)
  pbp$prob_2pt <- two_pt_conversion_prob(pbp)

  # probability of making PAT: the field goal model's make probability from
  # the 15 for the roof/era group (pat_make_prob, built in
  # data-raw/build_sysdata.R)
  pbp$prob_1pt <- unname(pat_make_prob[as.character(pbp$fg_model_roof)])

  # in overtime the rules decide whether the game goes on after the try. The
  # three tries (0, 1 and 2 points) are stacked, each in the order of pbp
  tries <- lapply(c(0, 1, 2), function(pts) {
    d <- ot_touchdown(pbp, pts)
    d$pts <- rep(pts, n)
    d$score_differential <- if (pts == 0) {
      -d$score_differential
    } else {
      -d$score_differential - pts
    }
    d
  })
  probs <- do.call(vctrs::vec_rbind, tries)
  # switch posteam
  probs$posteam <- case_when(
    probs$home_team == probs$posteam ~ probs$away_team,
    probs$away_team == probs$posteam ~ probs$home_team
  )
  # 1st & 10 after the kickoff
  probs$yardline_100 <- kickoff_yardline(probs$season)
  probs$down <- rep(1, nrow(probs))
  probs$ydstogo <- rep(10, nrow(probs))
  w <- calculate_win_probability(flip_half(probs))$vegas_wp

  i <- seq_len(n)
  pbp$wp_0 <- w[i]
  pbp$wp_1 <- w[n + i]
  pbp$wp_2 <- w[2L * n + i]
  pbp$conv_2pt <- pbp$prob_2pt
  pbp$conv_1pt <- pbp$prob_1pt
  pbp$wp_go2 <- pbp$conv_2pt * pbp$wp_2 + (1 - pbp$conv_2pt) * pbp$wp_0
  pbp$wp_go1 <- pbp$conv_1pt * pbp$wp_1 + (1 - pbp$conv_1pt) * pbp$wp_0
  # convenience column useful for other things
  pbp$wp_td <- ifelse(pbp$wp_go1 > pbp$wp_go2, pbp$wp_go1, pbp$wp_go2)
  pbp$index_2pt <- NULL
  pbp
}

# go for it on 4th down WP
get_go_wp <- function(pbp) {
  pbp$go_index <- seq_len(nrow(pbp))

  # distribution of yards gained when going for it (see R/go_model.R), which
  # converts more often in late and desperate situations. A gain equal to
  # yardline_100 is a touchdown. Every outcome of a play is a row
  gains <- fd_gain_probs(pbp, late = fd_late(pbp))
  p <- data.frame(
    go_index = pbp$go_index[gains$row],
    gain = as.integer(gains$gain),
    prob = gains$prob
  )
  j <- match(p$go_index, pbp$go_index)
  for (n in setdiff(names(pbp), "go_index")) {
    p[[n]] <- pbp[[n]][j]
  }

  # update situation based on play result (every step is row by row)
  p$yardline_100 <- p$yardline_100 - p$gain
  # for figuring out if it was a td later
  p$final_yardline <- p$yardline_100
  p$turnover <- ifelse(p$gain < p$ydstogo, 1, 0)
  p$down <- rep(1, nrow(p))

  # overtime: a team that trails in the second possession loses when it
  # turns the ball over, otherwise the other team's possession starts
  p$ot_end <- if_else(
    p$turnover == 1 &
      p$qtr > 4 &
      p$ot_rule != "SD" &
      p$ot_possession == 2 &
      p$score_differential < 0,
    0,
    p$ot_end
  )
  p$ot_possession <- case_when(
    p$turnover == 0 | p$qtr <= 4 ~ p$ot_possession,
    p$ot_rule == "SD" ~ 3,
    TRUE ~ pmin(p$ot_possession + 1, 3)
  )

  # flip a bunch of columns on turnover on downs where other team gets ball
  # # note: touchdowns are dealt with separately later
  p$yardline_100 <- ifelse(
    p$turnover == 1,
    100 - p$yardline_100,
    p$yardline_100
  )
  # switch posteam if turnover
  p$posteam <- case_when(
    p$home_team == p$posteam & p$turnover == 1 ~ p$away_team,
    p$away_team == p$posteam & p$turnover == 1 ~ p$home_team,
    TRUE ~ p$posteam
  )
  # swap score diff if turnover on downs
  p$score_differential <- ifelse(
    p$turnover == 1,
    -p$score_differential,
    p$score_differential
  )
  # give 6 points for the TD plays
  p$score_differential <- ifelse(
    p$yardline_100 == 0,
    p$score_differential + 6,
    p$score_differential
  )

  # the clock of the next snap. A turnover on downs or a touchdown stops the
  # clock, so the next state comes 6 seconds (the play) later. After a
  # conversion that doesn't score the clock keeps running: the typical gap to
  # the offense's next snap (conversion_runoff()), or 6 seconds plus `runoff`
  # when it is supplied
  # Late in a half the clock after a conversion depends on timeouts. Without
  # one, the clock often runs out before another snap (conversion_clock_out();
  # the gap in conversion_runoff() is that of the conversions followed by a
  # snap), so each such conversion is also valued with the clock run out (an
  # extra row) and the two are weighted by that share. A team with a timeout can
  # instead stop the clock right after the play: only the play's own time runs
  # off (conversion_play_time(), longer for longer gains) and the timeout is
  # spent. In the last 2:00 of the 2nd and 4th quarters each conversion is also
  # valued that way, and the team takes the better of the two (below)
  conv <- p$turnover == 0 & p$yardline_100 > 0 & is.na(p$runoff)
  h0 <- p$half_seconds_remaining
  late <- conv &
    h0 <= 45 &
    (p$qtr == 2 | (p$qtr == 4 & p$score_differential <= 0))
  q <- ifelse(late %in% TRUE, conversion_clock_out(h0), 0)
  ro <- which(q > 0)
  home_ball <- p$posteam == p$home_team
  to_left <- ifelse(
    home_ball,
    p$home_timeouts_remaining,
    p$away_timeouts_remaining
  )
  stop <- which(conv & p$qtr %in% c(2, 4) & h0 <= 120 & to_left > 0)
  n0 <- nrow(p)
  tw <- n0 + seq_along(stop)
  rw <- n0 + length(stop) + seq_along(ro)
  if (length(stop) + length(ro) > 0) {
    p <- p[c(seq_len(n0), stop, ro), , drop = FALSE]
    h <- home_ball[stop]
    p$home_timeouts_remaining[tw] <- p$home_timeouts_remaining[tw] - h
    p$away_timeouts_remaining[tw] <- p$away_timeouts_remaining[tw] - !h
  }
  elapsed <- case_when(
    p$turnover == 1 | p$yardline_100 <= 0 ~ 6,
    is.na(p$runoff) ~ conversion_runoff(
      p$qtr,
      p$half_seconds_remaining,
      p$score_differential
    ),
    TRUE ~ 6 + p$runoff
  )
  elapsed[tw] <- conversion_play_time(p$gain[tw])
  elapsed[rw] <- p$half_seconds_remaining[rw]
  p$half_seconds_remaining <- pmax(p$half_seconds_remaining - elapsed, 0)
  p$game_seconds_remaining <- pmax(p$game_seconds_remaining - elapsed, 0)

  # if now goal to go for either team, use yardline for yards to go,
  # otherwise it's 1st and 10
  p$ydstogo <- ifelse(p$yardline_100 < 10, p$yardline_100, 10)

  # WP after a TD, from the choice of a 1pt or 2pt try
  td <- which(p$yardline_100 == 0)
  if (length(td) > 0) {
    tds <- get_2pt_wp(p[td, , drop = FALSE])
    p$wp_td <- tds$wp_td[match(seq_len(nrow(p)), td)]
  }

  preds <- calculate_win_probability(flip_half(p))
  # get the TD probs computed separately
  preds$vegas_wp <- ifelse(preds$yardline_100 == 0, preds$wp_td, preds$vegas_wp)
  # fill in end of game situation when team can kneel out clock after
  # successful non-td conversion
  preds$defteam_timeouts_remaining <- ifelse(
    preds$posteam == preds$home_team,
    preds$away_timeouts_remaining,
    preds$home_timeouts_remaining
  )
  sd <- preds$score_differential
  g <- preds$game_seconds_remaining
  dto <- preds$defteam_timeouts_remaining
  to <- preds$turnover
  yl <- preds$yardline_100
  preds$vegas_wp <- case_when(
    preds$qtr > 4 ~ preds$vegas_wp,
    sd > 0 & to == 0 & yl > 0 & g < 120 & dto == 0 ~ 1,
    sd > 0 & to == 0 & yl > 0 & g < 80 & dto == 1 ~ 1,
    sd > 0 & to == 0 & yl > 0 & g < 40 & dto == 2 ~ 1,
    TRUE ~ preds$vegas_wp
  )
  # fill in end of game situation when other team can kneel out clock after
  # failed attempt
  preds$vegas_wp <- case_when(
    preds$qtr > 4 ~ preds$vegas_wp,
    sd > 0 & to == 1 & g < 120 & dto == 0 ~ 0,
    sd > 0 & to == 1 & g < 80 & dto == 1 ~ 0,
    sd > 0 & to == 1 & g < 40 & dto == 2 ~ 0,
    TRUE ~ preds$vegas_wp
  )

  # without a timeout the clock may run out after a conversion; a team with one
  # uses it if that is better
  if (length(stop) + length(ro) > 0) {
    wp <- preds$vegas_wp
    wp[ro] <- (1 - q[ro]) * wp[ro] + q[ro] * wp[rw]
    wp[stop] <- pmax(wp[stop], wp[tw])
    preds$vegas_wp <- wp
    preds <- preds[seq_len(n0), , drop = FALSE]
  }

  # by play and outcome (converted or not): the probability, and the WP
  # weighted by the outcomes' probability within it. Sums by group in row
  # order, as group_by() and summarize() did
  fd_pct <- stats::ave(preds$prob, preds$go_index, preds$turnover, FUN = sum)
  wt_within <- preds$prob / fd_pct * preds$vegas_wp
  by <- list(preds$go_index, preds$turnover)
  pct <- tapply(preds$prob, by, sum)
  wp <- tapply(wt_within, by, sum)
  col <- function(m, k) if (k %in% colnames(m)) m[, k] else NA_real_
  i <- match(pbp$go_index, as.integer(rownames(pct)))
  pbp$first_down_prob <- unname(col(pct, "0")[i])
  pbp$wp_fail <- unname(col(wp, "1")[i])
  pbp$wp_succeed <- unname(col(wp, "0")[i])

  go <- tapply(preds$prob * preds$vegas_wp, preds$go_index, sum)
  pbp$go_wp <- as.vector(go)[match(pbp$go_index, as.integer(names(go)))]
  pbp$go_index <- NULL
  pbp
}
