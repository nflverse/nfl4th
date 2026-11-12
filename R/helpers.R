# # # # a couple basic helpers

# for standardizing team names
team_name_fn <- function(var) {
  # each replacement in turn, as stringr::str_replace_all() with a named vector
  fix <- c(
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
      home_timeouts_remaining = if_else(
        home,
        df$posteam_timeouts_remaining,
        df$defteam_timeouts_remaining
      ),
      away_timeouts_remaining = if_else(
        df$posteam == df$away_team,
        df$posteam_timeouts_remaining,
        df$defteam_timeouts_remaining
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
  df$posteam <- if_else(df$home_team == df$posteam, df$away_team, df$home_team)
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
# 2016-2026 regular and postseason games (data-raw/_conversion_runoff.R), linear
# in the seconds left in the half between the points measured. In the last 2:00
# of the 2nd and of the 4th quarter (behind or tied) they leave out conversions
# followed by an offense timeout, which a team with one can take instead (see
# conversion_play_time() and get_go_wp()); earlier, teams take one after at most
# 3% of conversions. A later conversion never leads to an earlier next snap.
# Overtime uses the 4th quarter's.
conversion_runoff <- function(qtr, half_seconds_remaining, score_differential) {
  n <- max(
    length(qtr),
    length(half_seconds_remaining),
    length(score_differential)
  )
  qtr <- rep_len(qtr, n)
  score_differential <- rep_len(score_differential, n)
  h <- rep_len(half_seconds_remaining, n)
  at <- function(x, y) stats::approx(x, y, xout = h, rule = 2)$y
  q2 <- at(
    c(0, 22, 37, 52, 75, 105, 135, 165, 210, 900),
    c(5.9, 8.9, 11.4, 14.6, 18.2, 21.1, 17, 36, 36.5, 36.6)
  )
  q4_behind <- at(
    c(0, 22, 37, 52, 75, 105, 135, 165, 210, 270, 330, 420, 540, 750, 900),
    c(
      7.2,
      12.1,
      14.8,
      15.5,
      18.4,
      20.4,
      13,
      23,
      27,
      25,
      31,
      32.3,
      33.7,
      34.4,
      34.4
    )
  )
  q4_ahead <- at(
    c(0, 75, 105, 135, 165, 210, 270, 330, 420, 900),
    c(35, 37.5, 31.8, 15, 25, 29, 32, 40, 39.5, 39)
  )
  case_when(
    qtr %in% c(1, 3) ~ 36.6,
    qtr == 2 ~ q2,
    score_differential > 0 ~ q4_ahead,
    TRUE ~ q4_behind
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
  stats::approx(
    c(3.5, 8.5, 12.5, 17.5, 25, 37.5, 45),
    c(0.84, 0.65, 0.44, 0.10, 0.012, 0.009, 0),
    xout = half_seconds_remaining,
    rule = 2
  )$y
}

# Seconds a play that gains a first down takes when the offense stops the clock
# with a timeout right after it: about 6 seconds plus 0.107 per yard gained
# (plays stopped by an offense timeout in the last 2:00 of a half, 2016-2026,
# data-raw/_conversion_runoff.R)
conversion_play_time <- function(gain) {
  5.99 + 0.107 * gain
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
  if (anyNA(season)) {
    cli::cli_abort("{.arg season} must not contain missing values.")
  }
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
  if (anyNA(season)) {
    cli::cli_abort("{.arg season} must not contain missing values.")
  }
  if_else(season >= 2020, 1L, 0L)
}

# helper function to move the game to start of 3rd Q on an end-of-half play
# on the plays where we find that the half has ended
flip_half <- function(df) {
  prior_posteam <- df$posteam
  end_of_half <- ifelse(df$qtr == 2 & df$half_seconds_remaining == 0, 1, 0)
  df$posteam <- case_when(
    df$home_opening_kickoff == 1 & end_of_half == 1 ~ df$away_team,
    df$home_opening_kickoff == 0 & end_of_half == 1 ~ df$home_team,
    TRUE ~ df$posteam
  )
  df$qtr <- ifelse(end_of_half == 1, 3L, df$qtr)
  df$down <- ifelse(end_of_half == 1, 1, df$down)
  df$ydstogo <- ifelse(end_of_half == 1, 10L, df$ydstogo)
  df$yardline_100 <- ifelse(
    end_of_half == 1,
    kickoff_yardline(df$season),
    df$yardline_100
  )
  df$half_seconds_remaining <- ifelse(
    end_of_half == 1,
    1800,
    df$half_seconds_remaining
  )
  df$game_seconds_remaining <- ifelse(
    end_of_half == 1,
    1800,
    df$game_seconds_remaining
  )
  df$score_differential <- ifelse(
    df$posteam != prior_posteam & end_of_half == 1,
    -df$score_differential,
    df$score_differential
  )
  df$home_receive_2h_ko <- ifelse(end_of_half == 1, 0, df$home_receive_2h_ko)
  df
}

# fill in end of game situation when team can kneel out clock
# discourages punting or fg when the other team can end the game
end_game_fn <- function(pbp) {
  pbp$defteam_timeouts_remaining <- ifelse(
    pbp$posteam == pbp$home_team,
    pbp$away_timeouts_remaining,
    pbp$home_timeouts_remaining
  )
  sd <- pbp$score_differential
  g <- pbp$game_seconds_remaining
  dto <- pbp$defteam_timeouts_remaining
  pbp$vegas_wp <- case_when(
    pbp$qtr > 4 ~ pbp$vegas_wp,
    sd > 0 & g < 120 & dto == 0 ~ 0,
    sd > 0 & g < 80 & dto == 1 ~ 0,
    sd > 0 & g < 40 & dto == 2 ~ 0,
    TRUE ~ pbp$vegas_wp
  )
  pbp
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
    list(type = ifelse(tolower(pbp$season_type) == "reg", "reg", "post"))
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
# column the package code names)
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

# convenience function to add the probs from each model. The decision
# functions expand every play into its outcomes, so they work on only the
# columns they read (nflfastR play-by-play has hundreds more), and the results
# are attached to the full rows at the end
add_probs <- function(df) {
  # a plain data.frame: tibble's checks on every column assignment add up
  # the columns as a plain data.frame, without `[` on the caller's data frame
  # (an nflreadr data frame is also a data.table)
  keep <- df_cols(df)[names(df) %in% decision_cols]
  out <- vctrs::new_data_frame(keep, n = .row_names_info(df, 2L)) |>
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
