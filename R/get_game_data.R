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
#' \item{The rest}{All the columns needed for `add_4th_probs().`}
#' }
#' @export
#' @examples
#' \donttest{
#' plays <- nfl4th::get_4th_plays('2020_20_TB_GB')
#'
#' dplyr::glimpse(plays)
#' }
get_4th_plays <- function(gid) {

  df <- .games_nfl4th %>%
    filter(game_id == gid)

  plays <- data.frame()

  tryCatch(
    expr = {


      pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={df$espn}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if ("code" %in% names(pbp)) {
        warning(warn <- 1)
      }

      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be ok to cut this
      if ("current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        current_drive <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)

        previous_drives <- pbp$drives$previous

        drives <- bind_rows(
          previous_drives %>% dplyr::select(team.abbreviation, plays) %>% tidyr::unnest(plays),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- previous_drives %>% dplyr::select(team.abbreviation, plays) %>% tidyr::unnest(plays)
      }

      suppressWarnings(

        plays <- drives %>%
          as_tibble() %>%
          group_by(id) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          janitor::clean_names() %>%
          dplyr::rename(
            posteam = team_abbreviation,
            qtr = period_number,
            yardline_100 = start_yards_to_endzone,
            yardline = start_possession_text,
            down = start_down,
            ydstogo = start_distance,
            desc = text,
            time = clock_display_value
          ) %>%
          dplyr::filter(qtr <= 4) %>%
          dplyr::mutate(
            # time column is wacky so extract it from play description when possible
            play_time = stringr::str_extract(desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(play_time, 2, nchar(play_time)),
            play_min = stringr::str_extract(play_time, "[^()]+(?=\\:)") %>% as.integer(),
            play_min = if_else(is.na(play_min) & !is.na(play_time), as.integer(0), play_min),
            play_sec = substr(play_time, nchar(play_time) - 1, nchar(play_time)) %>% as.integer(),
            mins = if_else(nchar(time) == 5, substr(time, 1, 2), substr(time, 1, 1)) %>% as.integer(),
            secs = if_else(nchar(time) == 5, substr(time, 4, 5), substr(time, 3, 4)) %>% as.integer(),
            mins = if_else(is.na(play_min), mins, play_min),
            secs = if_else(is.na(play_sec), secs, play_sec)
          ) %>%
          arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            home_team = df$home_team,
            away_team = df$away_team,
            posteam = case_when(
              posteam == "WSH" ~ "WAS",
              posteam == "LAR" ~ "LA",
              TRUE ~ posteam
            )
          ) %>%
          dplyr::mutate_at(dplyr::vars("home_team", "away_team", "posteam"), team_name_fn) %>%
          mutate(
            defteam = if_else(posteam == home_team, away_team, home_team),
            half = if_else(qtr <= 2, 1, 2),
            challenge_team = stringr::str_extract(desc, "[:alpha:]*\\s*[:alpha:]*\\s*[:alpha:]*[:alpha:]+(?=\\schallenged)"),
            challenge_team = stringr::str_replace_all(challenge_team, "[\r\n]" , ""),
            challenge_team = stringr::str_trim(challenge_team, side = c("both")),
            desc_timeout = if_else(stringr::str_detect(desc, "Timeout #[:digit:]"), 1, 0),
            timeout_team = stringr::str_extract(desc, "(?<=Timeout #[:digit:] by )[:upper:]{2,3}"),
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
              challenge_team == "Los Angeles Chargers" & desc_timeout == 1 ~ "LAC",
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
            home_timeouts_remaining = 3,
            away_timeouts_remaining = 3
          ) %>%
          dplyr::group_by(half) %>%
          arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            total_home_timeouts_used = dplyr::if_else(cumsum(home_timeout_used) > 3, 3, cumsum(home_timeout_used)),
            total_away_timeouts_used = dplyr::if_else(cumsum(away_timeout_used) > 3, 3, cumsum(away_timeout_used))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
            away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
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
            score_differential = if_else(posteam == home_team, home_score - away_score, away_score - home_score),
            runoff = 0,
            season = df$season,
            home_opening_kickoff = if_else(dplyr::first(stats::na.omit(posteam)) == home_team, 1, 0),
            type = df$type
          ) %>%
          filter(
            down == 4,
            !(time < 30 & qtr %in% c(4)),
            is.na(timeout_team),
            type_text != "Two-minute warning",
            type_text != "End Period"
          ) %>%
          group_by(qtr, time, ydstogo) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          arrange(qtr, desc(time), ydstogo) %>%
          mutate(
            game_id = df$game_id,
            espn_id = df$espn,
            yardline_side = purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[1]
            ),
            yardline_side = case_when(
              yardline_side == "WSH" ~ "WAS",
              yardline_side == "LAR" ~ "LA",
              TRUE ~ yardline_side
            ),
            yardline_number = as.numeric(purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[2]
            )),
            temp_yardline = dplyr::if_else(
              yardline_side == posteam | yardline_100 == 50,
              100 - yardline_number,
              yardline_number
            ),
            yardline_100 = if_else(
              !is.na(temp_yardline), as.integer(temp_yardline), yardline_100
            )
          ) %>%
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
            season
          ) %>%
          # put in end of game conditions
          dplyr::mutate(
            # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
            runoff = if_else(between(quarter_seconds_remaining, 167, 300) & score_differential > 0 & qtr == 4, 40, runoff),
            # if there's a conversion right before 2 minute warning, run down to 2 minute warning
            runoff = if_else(between(quarter_seconds_remaining, 127, 166) & score_differential > 0 & qtr == 4, quarter_seconds_remaining - 120 - 6, runoff),
            # if conversion after 2 minute warning, run down 40 seconds
            runoff = if_else(quarter_seconds_remaining <= 120 & score_differential > 0 & qtr == 4, 40, runoff)
          )
      )

      if (nrow(plays) > 0) {
        plays <- plays %>%
          mutate(
            index = 1 : n()
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
        message(glue::glue("Warning: The requested GameID {df$espn} ({df$game_id}) is invalid!"))
      }
    },
    finally = {
    }

  )

  return(plays)
}
