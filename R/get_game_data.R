################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function that can pull 4th downs from a live game courtesy of ESPN
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down plays from a game
#'
#' @description Get 4th down plays from a game.
#'
#' @param gid A game to get 4th down decisions of.
#' @details Obtains a data frame that can be used with `add_4th_probs()`. The following columns
#' must be present:
#' \itemize{
#' \item{game_id : game ID in nflfastR format (eg '2020_20_TB_GB')}
#' }
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#' \item{desc}{Play description from ESPN.}
#' \item{type_text}{Play type text from ESPN.}
#' \item{index}{Index number of play from a given game. Useful for tracking plays (e.g. for 4th down bot).}
#' \item{ot_possession}{Overtime possession number (1, 2, or 3 for any later possession), `NA` in regulation.}
#' \item{The rest}{All the columns needed for `add_4th_probs().`}
#' }
#' @export
#' @examples
#' \donttest{
#' try({# to avoid cran issues
#' plays <- nfl4th::get_4th_plays('2020_20_TB_GB')
#'
#' dplyr::glimpse(plays)
#' })
#' }
get_4th_plays <- function(gid) {
  df <- .games_nfl4th() |>
    filter(game_id == gid)

  plays <- data.frame()

  tryCatch(
    expr = {
      game_url <- paste0(
        "http://site.api.espn.com/apis/site/v2/sports/football/nfl/",
        "summary?event=",
        df$espn
      )
      pbp <- game_url |>
        jsonlite::fromJSON(flatten = TRUE)

      if ("code" %in% names(pbp)) {
        warning(warn <- 1)
      }

      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be
      # ok to cut this
      if (
        "current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)
      ) {
        current_drive <- pbp$drives$current
        current_drive <- current_drive[['plays']] |>
          bind_rows() |>
          as_tibble() |>
          mutate(team.abbreviation = current_drive$team$abbreviation)

        previous_drives <- pbp$drives$previous

        drives <- bind_rows(
          unnest_plays(previous_drives),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] |>
          bind_rows() |>
          as_tibble() |>
          mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- unnest_plays(previous_drives)
      }

      suppressWarnings(
        plays <- drives |>
          as_tibble() |>
          group_by(id) |>
          dplyr::slice(1) |>
          ungroup() |>
          clean_names() |>
          dplyr::rename(
            posteam = team_abbreviation,
            qtr = period_number,
            yardline_100 = start_yards_to_endzone,
            yardline = start_possession_text,
            down = start_down,
            ydstogo = start_distance,
            desc = text,
            time = clock_display_value
          ) |>
          dplyr::mutate(
            # time column is wacky so extract it from play description when
            # possible
            play_time = re_extract(desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(play_time, 2, nchar(play_time)),
            play_min = re_extract(play_time, "[^()]+(?=\\:)") |>
              as.integer(),
            play_min = if_else(
              is.na(play_min) & !is.na(play_time),
              as.integer(0),
              play_min
            ),
            play_sec = substr(
              play_time,
              nchar(play_time) - 1,
              nchar(play_time)
            ) |>
              as.integer(),
            mins = if_else(
              nchar(time) == 5,
              substr(time, 1, 2),
              substr(time, 1, 1)
            ) |>
              as.integer(),
            secs = if_else(
              nchar(time) == 5,
              substr(time, 4, 5),
              substr(time, 3, 4)
            ) |>
              as.integer()
            # trying to fix wacky behavior 9/2023
            # mins = if_else(is.na(play_min), mins, play_min),
            # secs = if_else(is.na(play_sec), secs, play_sec)
          ) |>
          arrange(qtr, desc(mins), desc(secs), id) |>
          dplyr::mutate(
            home_team = df$home_team,
            away_team = df$away_team,
            posteam = case_when(
              posteam == "WSH" ~ "WAS",
              posteam == "LAR" ~ "LA",
              TRUE ~ posteam
            )
          ) |>
          dplyr::mutate_at(
            dplyr::vars("home_team", "away_team", "posteam"),
            team_name_fn
          ) |>
          mutate(
            defteam = if_else(posteam == home_team, away_team, home_team),
            # overtime periods count their timeouts on their own
            half = dplyr::case_when(qtr <= 2 ~ 1, qtr <= 4 ~ 2, TRUE ~ qtr),
            challenge_team = re_extract(
              desc,
              paste0(
                "[[:alpha:]]*\\s*[[:alpha:]]*\\s*[[:alpha:]]*",
                "[[:alpha:]]+(?=\\schallenged)"
              )
            ),
            challenge_team = gsub("[\r\n]", "", challenge_team),
            challenge_team = trimws(challenge_team),
            desc_timeout = if_else(
              re_detect(desc, "Timeout #[[:digit:]]"),
              1,
              0
            ),
            timeout_team = re_extract(
              desc,
              "(?<=Timeout #[[:digit:]] by )[[:upper:]]{2,3}"
            ),
            timeout_team = case_when(
              # fix team abbrevs
              timeout_team == "WSH" ~ "WAS",
              timeout_team == "LAR" ~ "LA",
              timeout_team == "ARZ" ~ "ARI",
              timeout_team == "BLT" ~ "BAL",
              timeout_team == "CLV" ~ "CLE",
              timeout_team == "HST" ~ "HOU",
              timeout_team == "SL" ~ "LA",

              # fill in timeouts charged on challenges
              challenge_team == "Arizona" & desc_timeout == 1 ~ "ARI",
              challenge_team == "Atlanta" & desc_timeout == 1 ~ "ATL",
              challenge_team == "Baltimore" & desc_timeout == 1 ~ "BAL",
              challenge_team == "Buffalo" & desc_timeout == 1 ~ "BUF",
              challenge_team == "Carolina" & desc_timeout == 1 ~ "CAR",
              challenge_team == "Chicago" & desc_timeout == 1 ~ "CHI",
              challenge_team == "Cincinnati" & desc_timeout == 1 ~ "CIN",
              challenge_team == "Cleveland" & desc_timeout == 1 ~ "CLE",
              challenge_team == "Dallas" & desc_timeout == 1 ~ "DAL",
              challenge_team == "Denver" & desc_timeout == 1 ~ "DEN",
              challenge_team == "Detroit" & desc_timeout == 1 ~ "DET",
              challenge_team == "Green Bay" & desc_timeout == 1 ~ "GB",
              challenge_team == "Houston" & desc_timeout == 1 ~ "HOU",
              challenge_team == "Indianapolis" & desc_timeout == 1 ~ "IND",
              challenge_team == "Jacksonville" & desc_timeout == 1 ~ "JAX",
              challenge_team == "Kansas City" & desc_timeout == 1 ~ "KC",
              challenge_team == "Los Angeles Rams" & desc_timeout == 1 ~ "LA",
              challenge_team == "Los Angeles Chargers" &
                desc_timeout == 1 ~ "LAC",
              challenge_team == "Las Vegas" & desc_timeout == 1 ~ "LV",
              challenge_team == "Miami" & desc_timeout == 1 ~ "MIA",
              challenge_team == "Minnesota" & desc_timeout == 1 ~ "MIN",
              challenge_team == "New England" & desc_timeout == 1 ~ "NE",
              challenge_team == "New Orleans" & desc_timeout == 1 ~ "NO",
              challenge_team == "New York Giants" & desc_timeout == 1 ~ "NYG",
              challenge_team == "New York Jets" & desc_timeout == 1 ~ "NYJ",
              challenge_team == "Philadelphia" & desc_timeout == 1 ~ "PHI",
              challenge_team == "Pittsburgh" & desc_timeout == 1 ~ "PIT",
              challenge_team == "Seattle" & desc_timeout == 1 ~ "SEA",
              challenge_team == "San Francisco" & desc_timeout == 1 ~ "SF",
              challenge_team == "Tampa Bay" & desc_timeout == 1 ~ "TB",
              challenge_team == "Tennessee" & desc_timeout == 1 ~ "TEN",
              challenge_team == "Washington" & desc_timeout == 1 ~ "WAS",

              TRUE ~ timeout_team
            ),
            home_timeout_used = case_when(
              timeout_team == home_team ~ 1,
              timeout_team != home_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            away_timeout_used = case_when(
              timeout_team == away_team ~ 1,
              timeout_team != away_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            # two timeouts per team in overtime (the overtime win probability
            # model does not use them)
            home_timeouts_remaining = if_else(qtr > 4, 2, 3),
            away_timeouts_remaining = if_else(qtr > 4, 2, 3)
          ) |>
          dplyr::group_by(half) |>
          arrange(qtr, desc(mins), desc(secs), id) |>
          dplyr::mutate(
            total_home_timeouts_used = dplyr::if_else(
              cumsum(home_timeout_used) > 3,
              3,
              cumsum(home_timeout_used)
            ),
            total_away_timeouts_used = dplyr::if_else(
              cumsum(away_timeout_used) > 3,
              3,
              cumsum(away_timeout_used)
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            home_timeouts_remaining = home_timeouts_remaining -
              total_home_timeouts_used,
            away_timeouts_remaining = away_timeouts_remaining -
              total_away_timeouts_used,
            posteam_timeouts_remaining = dplyr::if_else(
              posteam == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            defteam_timeouts_remaining = dplyr::if_else(
              defteam == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            time = 60 * as.integer(mins) + as.integer(secs),
            home_score = dplyr::lag(home_score),
            away_score = dplyr::lag(away_score),
            score_differential = if_else(
              posteam == home_team,
              home_score - away_score,
              away_score - home_score
            ),
            # the typical gap to the next snap after a conversion (see
            # conversion_runoff())
            runoff = NA_real_,
            season = df$season,
            home_opening_kickoff = if_else(
              dplyr::first(stats::na.omit(posteam)) == home_team,
              1,
              0
            ),
            type = df$type,
            # overtime possession number, counted over the snaps
            game_id = df$game_id,
            down = dplyr::if_else(
              down %in%
                1:4 &
                !type_text %in%
                  c(
                    "Timeout",
                    "Two-minute warning",
                    "End Period",
                    "End of Game"
                  ),
              down,
              NA_integer_
            )
          ) |>
          ot_possession_from_pbp() |>
          filter(
            down == 4,
            !(time < 15 & qtr %in% c(4)),
            is.na(timeout_team),
            type_text != "Two-minute warning",
            type_text != "End Period"
          ) |>
          group_by(qtr, time, ydstogo) |>
          dplyr::slice(1) |>
          ungroup() |>
          arrange(qtr, desc(time), ydstogo) |>
          mutate(
            game_id = df$game_id,
            espn_id = df$espn,
            yardline_side = split_part(yardline, 1L),
            yardline_side = case_when(
              yardline_side == "WSH" ~ "WAS",
              yardline_side == "LAR" ~ "LA",
              TRUE ~ yardline_side
            ),
            yardline_number = as.numeric(split_part(yardline, 2L)),
            temp_yardline = dplyr::if_else(
              yardline_side == posteam | yardline_100 == 50,
              100 - yardline_number,
              yardline_number
            ),
            yardline_100 = if_else(
              !is.na(temp_yardline),
              as.integer(temp_yardline),
              yardline_100
            )
          ) |>
          select(
            game_id,
            espn_id,
            play_id = id,
            desc,
            type,
            qtr,
            quarter_seconds_remaining = time,
            posteam,
            away_team,
            home_team,
            # needed for bot text
            yardline,
            yardline_100,
            ydstogo,
            posteam_timeouts_remaining,
            defteam_timeouts_remaining,
            home_opening_kickoff,
            score_differential,
            runoff,
            home_score,
            away_score,
            type_text,
            season,
            ot_possession
          )
      )

      if (nrow(plays) > 0) {
        plays <- plays |>
          mutate(
            index = 1:n()
          )
      } else {
        plays$index <- NA_real_
      }
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        cli::cli_alert_warning(
          "Warning: The requested GameID {df$espn} ({df$game_id}) is invalid!"
        )
      }
    },
    finally = {}
  )

  return(plays)
}

# tidyr::unnest() of the plays of the previous drives: each drive's plays, with
# the drive's team
unnest_plays <- function(drives) {
  plays <- drives$plays
  n <- vapply(plays, NROW, 1L)
  out <- dplyr::bind_rows(plays[n > 0])
  dplyr::bind_cols(
    tibble::tibble(team.abbreviation = rep(drives$team.abbreviation, n)),
    out
  )
}

# janitor::clean_names() for the names ESPN uses (camelCase and dotted names to
# snake case; duplicates get _2, _3, ...)
clean_names <- function(df) {
  nm <- names(df)
  nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
  nm <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", nm)
  nm <- gsub("[^A-Za-z0-9]+", "_", nm)
  nm <- tolower(gsub("^_+|_+$", "", nm))
  for (u in unique(nm[duplicated(nm)])) {
    i <- which(nm == u)
    nm[i[-1]] <- paste0(u, "_", seq_along(i)[-1])
  }
  names(df) <- nm
  df
}

# the k-th piece of each string split at single spaces, as
# purrr::map_chr(stringr::str_split(x, " "), function(x) x[k]) (an empty string
# is one empty piece)
split_part <- function(x, k) {
  vapply(
    strsplit(x, " ", fixed = TRUE),
    function(p) if (length(p) == 0L) c("", NA)[k] else p[k],
    character(1)
  )
}
