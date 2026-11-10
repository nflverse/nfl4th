# Overtime
#
# Overtime states are valued with nflfastR's rule aware overtime model through
# nflfastR::calculate_win_probability() (no blend with nfl4th's own model,
# which only knows regulation). That model values the state of a
# possession that goes on. What happens when a possession ends depends on the
# rules and on how many possessions there have been, so it is handled here:
#
#   SD   - sudden death. First score wins. Regular season through 2011,
#          postseason through 2009.
#   MSD  - modified sudden death. A first possession TD wins, a first
#          possession FG lets the other team answer. Regular season
#          2012-2024, postseason 2010-2021.
#   BOTH - both teams are guaranteed a possession. Postseason 2022+, regular
#          season 2025+.
#
# `ot_possession` is 1 for the first possession of overtime, 2 for the second
# and 3 for any later one (sudden death from there on). `ot_end` is the final
# WP of the team with the 4th down decision when the rules end the game; it is
# NA when the game goes on.

ot_rule <- function(season, type) {
  post <- type %in% "post"
  dplyr::case_when(
    post & season >= 2022 ~ "BOTH",
    !post & season >= 2025 ~ "BOTH",
    post & season >= 2010 ~ "MSD",
    !post & season >= 2012 ~ "MSD",
    TRUE ~ "SD"
  )
}

# The overtime model is exported through nflfastR::calculate_win_probability()
# from the nflfastR version that added the `ot_possession` column
nflfastr_has_ot <- function() {
  exists(
    "calculate_ot_win_probability",
    envir = asNamespace("nflfastR"),
    inherits = FALSE
  )
}

# WP of the team with the ball in overtime states. `w` is
# nflfastR::calculate_win_probability()'s vegas_wp for these states
ot_win_probability <- function(x, w) {
  if (!nflfastr_has_ot()) {
    cli::cli_warn(c(
      "Overtime win probability needs a newer version of nflfastR.",
      "i" = "Overtime decisions are returned as {.val NA}."
    ))
    return(rep(NA_real_, nrow(x)))
  }
  # a regular season game ends when the clock runs out, a tie is half a win
  over <- x$type %in% "reg" & x$game_seconds_remaining <= 0
  d <- x$score_differential[over]
  w[over] <- ifelse(d > 0, 1, ifelse(d == 0, 0.5, 0))
  w
}

# The team with the ball gives it up without scoring (punt, missed field goal,
# turnover on downs). Call this before the possession is flipped: a team that
# trails in the second possession loses, and the other team's possession
# number goes up.
ot_change_possession <- function(df) {
  df |>
    mutate(
      ot_end = dplyr::if_else(
        qtr > 4 &
          ot_rule != "SD" &
          ot_possession == 2 &
          score_differential < 0,
        0,
        ot_end
      ),
      ot_possession = dplyr::case_when(
        qtr <= 4 ~ ot_possession,
        ot_rule == "SD" ~ 3,
        TRUE ~ pmin(ot_possession + 1, 3)
      )
    )
}

# The team with the ball makes a field goal. Call this before the possession is
# flipped (`score_differential` is still the kicking team's, before the kick).
ot_field_goal <- function(df) {
  df |>
    mutate(
      .d2 = score_differential + 3,
      ot_end = dplyr::case_when(
        qtr <= 4 ~ ot_end,
        ot_rule == "SD" | ot_possession >= 3 ~ 1,
        ot_possession == 2 & .d2 > 0 ~ 1,
        ot_possession == 2 & .d2 < 0 ~ 0,
        TRUE ~ ot_end
      ),
      # the other team receives: the second possession after a first
      # possession score, sudden death after a tying score
      ot_possession = dplyr::case_when(
        qtr <= 4 ~ ot_possession,
        ot_possession == 1 ~ 2,
        TRUE ~ 3
      )
    ) |>
    select(-".d2")
}

# The team with the ball scored a touchdown (`score_differential` includes the
# 6 points) and then `pts` on the try. Call this before the possession is
# flipped. A touchdown ends the game in sudden death, on the first possession
# under modified sudden death and on any later possession if it takes the lead.
ot_touchdown <- function(df, pts) {
  df |>
    mutate(
      .final = score_differential + pts,
      ot_end = dplyr::case_when(
        qtr <= 4 ~ ot_end,
        ot_rule == "SD" | ot_possession >= 3 ~ 1,
        ot_rule == "MSD" & ot_possession == 1 ~ 1,
        ot_possession == 2 & score_differential > 0 ~ 1,
        ot_possession == 2 & .final > 0 ~ 1,
        ot_possession == 2 & .final < 0 ~ 0,
        TRUE ~ ot_end
      ),
      ot_possession = dplyr::case_when(
        qtr <= 4 ~ ot_possession,
        ot_possession == 1 ~ 2,
        TRUE ~ 3
      )
    ) |>
    select(-dplyr::starts_with("."))
}

# Possession number in overtime from play-by-play data (sorted by game and
# play): 1 + the number of times the team with the ball changed among the
# overtime snaps so far (kickoffs and other plays without a down don't count),
# capped at 3. NA outside of overtime.
ot_possession_from_pbp <- function(pbp) {
  ot <- which(pbp$qtr > 4)
  poss <- rep(NA_real_, nrow(pbp))
  team <- ifelse(!is.na(pbp$down), pbp$posteam, NA_character_)
  for (idx in split(ot, pbp$game_id[ot])) {
    t <- team[idx]
    snap <- which(!is.na(t))
    p <- rep(NA_real_, length(idx))
    if (length(snap) > 0) {
      changed <- t[snap][-1] != t[snap][-length(snap)]
      p[snap] <- pmin(1 + cumsum(c(0, changed)), 3)
    }
    # rows without a snap take the value of the snap before them
    p <- data.table::nafill(p, type = "locf")
    p[is.na(p)] <- 1
    poss[idx] <- p
  }
  pbp$ot_possession <- poss
  pbp
}
