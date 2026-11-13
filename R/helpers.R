# # # # a couple basic helpers

# for standardizing team names
team_name_fn <- function(var) {
  # each replacement in turn, as stringr::str_replace_all() with a named vector
  fix <- c(
    "WSH" = "WAS",
    "LAR" = "LA",
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
  for (k in names(fix)) {
    var <- gsub(k, fix[[k]], var, fixed = TRUE)
  }
  var
}

# the first match of a (PCRE) pattern, NA where there is none or x is NA
re_extract <- function(x, pattern) {
  m <- regexpr(pattern, x, perl = TRUE)
  hit <- !is.na(m) & m > 0
  out <- rep(NA_character_, length(x))
  out[hit] <- substring(
    x[hit],
    m[hit],
    m[hit] + attr(m, "match.length")[hit] - 1L
  )
  out
}

# whether x matches a (PCRE) pattern, NA where x is NA
re_detect <- function(x, pattern) {
  out <- grepl(pattern, x, perl = TRUE)
  out[is.na(x)] <- NA
  out
}

# helper column to avoid join errors
drop.cols <- c(
  "game_id",
  "week",
  "model_roof",
  "roof",
  "era3",
  "era4",
  "home_total",
  "away_total",
  "total_line",
  "spread_line",
  "retractable",
  "dome",
  "outdoors"
)

# loading and cleaning games file
get_games_file <- function() {
  g <- nflreadr::load_schedules()
  g <- df_filter(g, g$season > 2013)
  season <- g$season
  roof <- g$roof
  model_roof <- case_when(
    roof == "open" | roof == "closed" | is.na(roof) ~ "retractable",
    roof == "retractable" ~ "retractable",
    roof == "dome" ~ "dome",
    TRUE ~ "outdoors"
  )
  # for field goal model
  fg_roof <- case_when(roof == "outdoors" ~ 1, TRUE ~ 0)
  fg_era <- get_fg_era(season)
  g <- df_assign(
    g,
    list(
      type = if_else(g$game_type == "REG", "reg", "post"),
      model_roof = model_roof,
      # for EP model: don't use this for games earlier than 2014
      era0 = 0,
      era1 = 0,
      era2 = 0,
      era3 = if_else(season > 2013 & season <= 2017, 1, 0),
      era4 = if_else(season > 2017, 1, 0),
      fg_roof = fg_roof,
      fg_era = fg_era,
      fg_model_roof = paste0(fg_roof, fg_era) |> as.factor(),
      home_total = (g$total_line + g$spread_line) / 2,
      away_total = (g$total_line - g$spread_line) / 2,
      retractable = if_else(model_roof == 'retractable', 1, 0),
      dome = if_else(model_roof == 'dome', 1, 0),
      outdoors = if_else(model_roof == 'outdoors', 1, 0),
      roof = model_roof,
      espn = case_when(
        g$game_id == "2025_20_BUF_DEN" ~ "401772982",
        g$game_id == "2025_20_SF_SEA" ~ "401772984",
        g$game_id == "2025_20_HOU_NE" ~ "401772983",
        g$game_id == "2025_20_LA_CHI" ~ "401772985",
        TRUE ~ g$espn
      )
    )
  )
  g <- df_assign(
    g,
    list(
      home_team = team_name_fn(g$home_team),
      away_team = team_name_fn(g$away_team)
    )
  )
  df_select(
    g,
    c(
      "game_id",
      "season",
      "type",
      "week",
      "away_team",
      "home_team",
      "espn",
      "fg_model_roof",
      "model_roof",
      "roof",
      "era0",
      "era1",
      "era2",
      "era3",
      "era4",
      "home_total",
      "away_total",
      "total_line",
      "spread_line",
      "retractable",
      "dome",
      "outdoors"
    )
  )
}

# data prep function
prepare_df <- function(df) {
  # if an nflfastR df is passed, need to drop all this so we don't get
  # redundant cols
  df <- df_select(df, setdiff(names(df), drop.cols))
  df <- df_left_join(
    df,
    .games_nfl4th(),
    by = c("home_team", "away_team", "type", "season")
  )
  qtr <- df$qtr
  qsr <- df$quarter_seconds_remaining
  # in overtime both are the time left in the overtime period
  half <- case_when(
    qtr > 4 ~ qsr,
    qtr == 2 | qtr == 4 ~ qsr,
    TRUE ~ qsr + 900
  )
  game <- if_else(qtr <= 2, half + 1800, half)
  # added
  elapsed_share <- (3600 - game) / 3600
  # useful for lots of stuff later
  home <- df$posteam == df$home_team
  df <- df_assign(
    df,
    list(
      home_receive_2h_ko = case_when(
        # home got it first
        qtr <= 2 & df$home_opening_kickoff == 1 ~ -1,
        # away got it first
        qtr <= 2 & df$home_opening_kickoff == 0 ~ 1,
        # it's the 2nd half
        TRUE ~ 0
      ),
      down = 4,
      half_seconds_remaining = half,
      game_seconds_remaining = game,
      elapsed_share = elapsed_share,
      spread_time = df$spread_line * exp(-4 * elapsed_share),
      posteam_spread = if_else(home, df$spread_line, -1 * df$spread_line),
      posteam_total = if_else(home, df$home_total, df$away_total),
      home_timeouts_remaining = timeouts_pos_def(df, home),
      away_timeouts_remaining = timeouts_pos_def(
        df,
        df$posteam == df$away_team
      ),
      original_posteam = df$posteam
    )
  )
  prepare_ot(df)
}

# overtime columns: the rules in force, the possession number (see
# R/overtime.R) and the final WP of states where the rules end the game
prepare_ot <- function(df) {
  if (!"ot_possession" %in% names(df)) {
    df$ot_possession <- NA_real_
  }
  ot <- df$qtr > 4
  guess <- ot & is.na(df$ot_possession)
  if (any(guess)) {
    cli::cli_warn(c(
      "{.field ot_possession} is missing for {sum(guess)} overtime play{?s}.",
      "i" = paste(
        "Assuming the first possession of overtime when the score is tied",
        "and the second when the team with the ball trails."
      ),
      "i" = paste(
        "Add {.field ot_possession} (1, 2 or 3 for any later possession)",
        "to get the right rules."
      )
    ))
  }
  df_assign(
    df,
    list(
      ot_rule = ot_rule(df$season, df$type),
      ot_possession = case_when(
        df$qtr <= 4 ~ NA_real_,
        !is.na(df$ot_possession) ~
          pmin(pmax(as.numeric(df$ot_possession), 1), 3),
        df$score_differential < 0 ~ 2,
        TRUE ~ 1
      ),
      ot_end = NA_real_
    )
  )
}

# the other team: the away team where `team` is the home team, the home team
# otherwise
other_team <- function(team, home_team, away_team) {
  if_else(team == home_team, away_team, home_team)
}

# Timeouts are carried two ways: by home and away team
# (home_/away_timeouts_remaining, which the decision functions update as the
# ball changes hands) and by the team with the ball and the other team
# (posteam_/defteam_timeouts_remaining, which the models read).
# The home team's timeouts where `home` is TRUE, the away team's elsewhere
timeouts_home_away <- function(df, home) {
  if_else(home, df$home_timeouts_remaining, df$away_timeouts_remaining)
}

# The timeouts of the team with the ball where `pos` is TRUE, the other team's
# elsewhere
timeouts_pos_def <- function(df, pos) {
  if_else(pos, df$posteam_timeouts_remaining, df$defteam_timeouts_remaining)
}

# helper function for switching possession and running off 6 seconds. In
# overtime a change of possession without a score also moves the possession
# number on (see R/overtime.R). After a score the caller does that instead
# (`ot_stop = FALSE`).
flip_team <- function(df, ot_stop = TRUE) {
  if (ot_stop) {
    df <- ot_change_possession(df)
  }
  n <- nrow(df)
  # switch posteam
  df$posteam <- other_team(df$posteam, df$home_team, df$away_team)
  # swap score
  df$score_differential <- -df$score_differential
  # 1st and 10
  df$down <- rep(1, n)
  df$ydstogo <- rep(10, n)
  # run off 6 seconds, but don't let seconds go negative
  h <- df$half_seconds_remaining - 6
  g <- df$game_seconds_remaining - 6
  df$half_seconds_remaining <- if_else(h < 0, 0, h)
  df$game_seconds_remaining <- if_else(g < 0, 0, g)
  df
}

# yardline_100 of the receiving team's first snap after a kickoff, by kickoff
# rule regime: the average first snap in each regime (the same spots nflfastR's
# win probability model uses). 2011-2015 kickoffs from the 35 with touchbacks
# at the 20, 2016-2023 touchbacks at the 25, the 2024 dynamic kickoff with
# touchbacks at the 30 and 2025+ with touchbacks at the 35.
kickoff_yardline <- function(season) {
  case_when(
    season >= 2025 ~ 69L,
    season >= 2024 ~ 70L,
    season >= 2016 ~ 75L,
    season >= 2011 ~ 78L,
    TRUE ~ 72L
  )
}

# Seconds from the snap of a successful 4th down conversion (not a touchdown) to
# the offense's next snap, which is the clock of the 1st down state that is
# valued, when the offense doesn't call a timeout. The clock doesn't stop for a
# first down, so away from the end of a half the next snap comes about 37
# seconds later. Late in a half teams hurry or stop the clock (out of bounds,
# spikes, the 2 minute warning, which is why the gap dips for conversions just
# before it). These are the mean gaps after 3rd and 4th down conversions in
# 2016-2026 regular and postseason games, in bins of seconds left in the half,
# taken at the middle of each bin and linear in between. They leave out
# conversions followed by an offense timeout, which a team with one can take
# instead late in a half (see conversion_play_time() and get_go_wp()); earlier,
# teams take one after at most 3% of conversions. A later conversion never
# leads to an earlier next snap. Overtime uses the 4th quarter's. The tables of
# these three functions are `conversion_clock` (R/sysdata.rda), written by
# data-raw/_conversion_runoff.R.
conversion_runoff <- function(qtr, half_seconds_remaining, score_differential) {
  n <- max(
    length(qtr),
    length(half_seconds_remaining),
    length(score_differential)
  )
  qtr <- rep_len(qtr, n)
  score_differential <- rep_len(score_differential, n)
  h <- rep_len(half_seconds_remaining, n)
  k <- conversion_clock$runoff
  at <- function(d) stats::approx(d$h, d$gap, xout = h, rule = 2)$y
  case_when(
    qtr %in% c(1, 3) ~ k$q1_q3,
    qtr == 2 ~ at(k$q2),
    score_differential > 0 ~ at(k$q4_ahead),
    TRUE ~ at(k$q4_behind)
  )
}

# Share of late conversions after which the clock runs out before another snap
# when the offense doesn't call a timeout, by seconds left in the half (2nd
# quarter, or 4th quarter behind or tied): 84% in the last 7 seconds, 65% at
# 7-10, 44% at 10-15, 10% at 15-20 and about 1% from 20 to 45 (conversions by
# teams without a timeout, 2016-2026, data-raw/_conversion_runoff.R). The gaps
# in conversion_runoff() are those of the conversions that were followed by a
# snap.
conversion_clock_out <- function(half_seconds_remaining) {
  k <- conversion_clock$clock_out
  stats::approx(k$h, k$share, xout = half_seconds_remaining, rule = 2)$y
}

# Seconds a play that gains a first down takes when the offense stops the clock
# with a timeout right after it: about 6 seconds plus 0.107 per yard gained
# (plays stopped by an offense timeout in the last 2:00 of a half, 2016-2026,
# data-raw/_conversion_runoff.R)
conversion_play_time <- function(gain) {
  k <- conversion_clock$play_time
  k[["intercept"]] + k[["per_yard"]] * gain
}

# Era bucket for the field goal model. Kicking has improved steadily rather
# than in steps: adjusting for distance, make probability has risen about 0.04
# log-odds per season since 2019. This is a recency bucket, not a rule-change
# boundary. Splitting at 2023 gives +0.22 log-odds indoor and +0.26 outdoor
# against earlier seasons, with both roofs well sampled either side.
# Used by both the prediction path and data-raw/_punt_and_fg_models.R, so it
# lives here rather than being written out twice.
# Callers come from the schedule and pbp files, where season is never missing,
# but an NA here would silently produce an fg_model_roof level the model has
# never seen, so fail loudly instead.
get_fg_era <- function(season) {
  check_season(season)
  if_else(season >= 2023, 1L, 0L)
}

# Era for the go for it model's conversion probability: 2001-2019 and 2020 on.
# Adjusting for distance, field position, team strength and the scoring
# environment, 4th down attempts convert about as often as 3rd downs through
# 2019 and more often since 2020 (57% vs 53% on 4th downs overall): more often
# at short and medium distances, slightly less at long ones, so the era shift
# depends on distance (see data-raw/_go_for_it_model.R). A smooth season trend
# or other splits did not do better. The latest era covers future seasons.
# Used by both the prediction path (R/go_model.R) and
# data-raw/_go_for_it_model.R. Like get_fg_era(), a missing season is an error.
get_fd_era <- function(season) {
  check_season(season)
  if_else(season >= 2020, 1L, 0L)
}

# the era functions' check: a missing season is an error (reported as the
# caller's)
check_season <- function(season, call = parent.frame()) {
  if (anyNA(season)) {
    cli::cli_abort(
      "{.arg season} must not contain missing values.",
      call = call
    )
  }
}

# helper function to move the game to start of 3rd Q on an end-of-half play
# on the plays where we find that the half has ended
flip_half <- function(df) {
  prior_posteam <- df$posteam
  end_of_half <- df$qtr == 2 & df$half_seconds_remaining == 0
  # the team that kicked off to start the game receives
  df$posteam <- case_when(
    df$home_opening_kickoff == 1 & end_of_half ~ df$away_team,
    df$home_opening_kickoff == 0 & end_of_half ~ df$home_team,
    TRUE ~ df$posteam
  )
  df$qtr <- if_else(end_of_half, 3L, df$qtr)
  df$down <- if_else(end_of_half, 1, df$down)
  df$ydstogo <- if_else(end_of_half, 10L, df$ydstogo)
  df$yardline_100 <- if_else(
    end_of_half,
    kickoff_yardline(df$season),
    df$yardline_100
  )
  df$half_seconds_remaining <- if_else(
    end_of_half,
    1800,
    df$half_seconds_remaining
  )
  df$game_seconds_remaining <- if_else(
    end_of_half,
    1800,
    df$game_seconds_remaining
  )
  df$score_differential <- if_else(
    df$posteam != prior_posteam & end_of_half,
    -df$score_differential,
    df$score_differential
  )
  df$home_receive_2h_ko <- if_else(end_of_half, 0, df$home_receive_2h_ko)
  df
}

# ##############################################################################
# other: this isn't used by the bot or shiny app but helps get an nflfastR df
# read

# prepare raw pbp data
prepare_nflfastr_data <- function(pbp) {
  # some prep. The possession number in overtime has to be counted before the
  # plays are filtered down to 4th downs
  data <- df_assign(
    pbp,
    list(type = if_else(tolower(pbp$season_type) == "reg", "reg", "post"))
  )
  data <- ot_possession_from_pbp(data)
  df_filter(
    data,
    (data$game_seconds_remaining > 15 | data$qtr > 4) &
      !is.na(data$half_seconds_remaining) &
      !is.na(data$qtr) &
      !is.na(data$posteam)
  )
}

# Columns the decision functions and models read (a superset: every prepared
# column the package code names). A column the code reads must be listed here:
# tests/testthat/test-helpers.R checks that add_probs() gives the same result
# on every column of nflfastR play-by-play.
decision_cols <- c(
  # the bootstrap draw of stacked plays (add_boot_probs())
  ".draw",
  "play_id",
  "home_team",
  "away_team",
  "season_type",
  "posteam",
  "defteam",
  "yardline_100",
  "quarter_seconds_remaining",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "qtr",
  "down",
  "time",
  "ydstogo",
  "desc",
  "extra_point_result",
  "two_point_conv_result",
  "home_timeouts_remaining",
  "away_timeouts_remaining",
  "timeout_team",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "score_differential",
  "fg_prob",
  "ep",
  "wp",
  "vegas_wp",
  "vegas_home_wp",
  "season",
  "play_type_nfl",
  "away_score",
  "home_score",
  "aborted_play",
  "pass",
  "rush",
  "id",
  "home_opening_kickoff",
  "index",
  "type",
  "ot_possession",
  "game_id",
  "week",
  "espn",
  "fg_model_roof",
  "model_roof",
  "roof",
  "era0",
  "era1",
  "era2",
  "era3",
  "era4",
  "home_total",
  "away_total",
  "total_line",
  "spread_line",
  "retractable",
  "dome",
  "outdoors",
  "home_receive_2h_ko",
  "elapsed_share",
  "posteam_spread",
  "posteam_total",
  "original_posteam",
  "ot_rule",
  "ot_end",
  "runoff"
)

# The columns of `df` in decision_cols, as a plain data.frame. The decision
# functions expand every play into its outcomes, so they work on only the
# columns they read (nflfastR play-by-play has hundreds more). A plain
# data.frame because tibble's checks on every column assignment add up, and
# built from the columns rather than with `[` on the caller's data frame (an
# nflreadr data frame is also a data.table, whose `[` follows other rules).
decision_frame <- function(df) {
  vctrs::new_data_frame(
    df_cols(df)[names(df) %in% decision_cols],
    n = .row_names_info(df, 2L)
  )
}

# convenience function to add the probs from each model, attached to the full
# rows at the end
add_probs <- function(df) {
  out <- decision_frame(df) |>
    get_go_wp() |>
    get_fg_wp() |>
    get_punt_wp()
  for (n in names(out)) {
    df[[n]] <- out[[n]]
  }
  df
}

# additional release issue bullets for usethis::use_release_issue()
release_bullets <- function() {
  c(
    '`devtools::check_mac_release()`',
    '`nflreadr:::my_rhub_check()`',
    '`pkgdown::check_pkgdown()`',
    '`nflreadr:::nflverse_thanks()`',
    NULL
  )
}
