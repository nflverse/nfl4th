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
#' tibble::glimpse(plays)
#' })
#' }
get_4th_plays <- function(gid) {
  df <- .games_nfl4th()
  df <- df_filter(df, df$game_id == gid)

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
        cli::cli_alert_warning(
          "Warning: The requested GameID {df$espn} ({df$game_id}) is invalid!"
        )
        return(plays)
      }

      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be
      # ok to cut this
      if (
        "current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)
      ) {
        current_drive <- pbp$drives$current
        current_drive <- current_plays(current_drive)

        previous_drives <- pbp$drives$previous

        drives <- df_bind_rows(list(
          unnest_plays(previous_drives),
          current_drive
        ))
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_plays(current_drive)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- unnest_plays(previous_drives)
      }

      suppressWarnings({
        plays <- tibble::as_tibble(drives)
        plays <- df_slice(plays, df_first_of_groups(list(plays$id)))
        plays <- clean_names(plays)
        plays <- df_rename(
          plays,
          c(
            posteam = "team_abbreviation",
            qtr = "period_number",
            yardline_100 = "start_yards_to_endzone",
            yardline = "start_possession_text",
            down = "start_down",
            ydstogo = "start_distance",
            desc = "text",
            time = "clock_display_value"
          )
        )

        # time column is wacky so extract it from play description when
        # possible
        play_time <- re_extract(plays$desc, "\\([^()]+(?=\\)\\s)")
        play_time <- substr(play_time, 2, nchar(play_time))
        play_min <- as.integer(re_extract(play_time, "[^()]+(?=\\:)"))
        plays$play_time <- play_time
        plays$play_min <- if_else(
          is.na(play_min) & !is.na(play_time),
          as.integer(0),
          play_min
        )
        plays$play_sec <- as.integer(substr(
          play_time,
          nchar(play_time) - 1,
          nchar(play_time)
        ))
        long <- nchar(plays$time) == 5
        plays$mins <- as.integer(if_else(
          long,
          substr(plays$time, 1, 2),
          substr(plays$time, 1, 1)
        ))
        plays$secs <- as.integer(if_else(
          long,
          substr(plays$time, 4, 5),
          substr(plays$time, 3, 4)
        ))
        # trying to fix wacky behavior 9/2023
        # mins = if_else(is.na(play_min), mins, play_min),
        # secs = if_else(is.na(play_sec), secs, play_sec)
        plays <- df_slice(
          plays,
          df_order(
            list(plays$qtr, plays$mins, plays$secs, plays$id),
            c(FALSE, TRUE, TRUE, FALSE)
          )
        )

        plays$home_team <- team_name_fn(df$home_team)
        plays$away_team <- team_name_fn(df$away_team)
        plays$posteam <- team_name_fn(plays$posteam)
        plays$defteam <- other_team(
          plays$posteam,
          plays$home_team,
          plays$away_team
        )
        # overtime periods count their timeouts on their own
        plays$half <- case_when(
          plays$qtr <= 2 ~ 1,
          plays$qtr <= 4 ~ 2,
          TRUE ~ plays$qtr
        )
        challenge_team <- re_extract(
          plays$desc,
          paste0(
            "[[:alpha:]]*\\s*[[:alpha:]]*\\s*[[:alpha:]]*",
            "[[:alpha:]]+(?=\\schallenged)"
          )
        )
        plays$challenge_team <- trimws(gsub("[\r\n]", "", challenge_team))
        plays$desc_timeout <- if_else(
          re_detect(plays$desc, "Timeout #[[:digit:]]"),
          1,
          0
        )
        # the team that called a timeout: ESPN's abbreviation, standardized,
        # or on a challenge that cost a timeout, the team that challenged
        timeout_team <- re_extract(
          plays$desc,
          "(?<=Timeout #[[:digit:]] by )[[:upper:]]{2,3}"
        )
        fixed <- team_name_fn(timeout_team)
        challenger <- unname(challenge_teams[plays$challenge_team])
        plays$timeout_team <- case_when(
          fixed != timeout_team ~ fixed,
          plays$desc_timeout == 1 & !is.na(challenger) ~ challenger,
          TRUE ~ timeout_team
        )
        plays$home_timeout_used <- case_when(
          plays$timeout_team == plays$home_team ~ 1,
          plays$timeout_team != plays$home_team ~ 0,
          is.na(plays$timeout_team) ~ 0
        )
        plays$away_timeout_used <- case_when(
          plays$timeout_team == plays$away_team ~ 1,
          plays$timeout_team != plays$away_team ~ 0,
          is.na(plays$timeout_team) ~ 0
        )

        # timeouts used so far in each half (in the row order above). Two
        # timeouts per team in overtime (the overtime win probability model
        # does not use them)
        half <- vctrs::vec_group_id(plays$half)
        home_used <- stats::ave(plays$home_timeout_used, half, FUN = cumsum)
        away_used <- stats::ave(plays$away_timeout_used, half, FUN = cumsum)
        plays$total_home_timeouts_used <- if_else(home_used > 3, 3, home_used)
        plays$total_away_timeouts_used <- if_else(away_used > 3, 3, away_used)
        plays$home_timeouts_remaining <- if_else(plays$qtr > 4, 2, 3) -
          plays$total_home_timeouts_used
        plays$away_timeouts_remaining <- if_else(plays$qtr > 4, 2, 3) -
          plays$total_away_timeouts_used
        plays$posteam_timeouts_remaining <- timeouts_home_away(
          plays,
          plays$posteam == plays$home_team
        )
        plays$defteam_timeouts_remaining <- timeouts_home_away(
          plays,
          plays$defteam == plays$home_team
        )

        plays$time <- 60 * as.integer(plays$mins) + as.integer(plays$secs)
        plays$home_score <- lag1(plays$home_score)
        plays$away_score <- lag1(plays$away_score)
        plays$score_differential <- if_else(
          plays$posteam == plays$home_team,
          plays$home_score - plays$away_score,
          plays$away_score - plays$home_score
        )
        # the typical gap to the next snap after a conversion (see
        # conversion_runoff())
        plays$runoff <- NA_real_
        plays$season <- df$season
        plays$home_opening_kickoff <- if_else(
          first_value(stats::na.omit(plays$posteam)) == plays$home_team,
          1,
          0
        )
        plays$type <- df$type
        plays$game_id <- df$game_id
        plays$down <- if_else(
          plays$down %in%
            1:4 &
            !plays$type_text %in%
              c(
                "Timeout",
                "Two-minute warning",
                "End Period",
                "End of Game"
              ),
          plays$down,
          NA_integer_
        )
        # overtime possession number, counted over the snaps
        plays <- ot_possession_from_pbp(plays)
        plays <- df_filter(
          plays,
          plays$down == 4 &
            !(plays$time < 15 & plays$qtr %in% c(4)) &
            is.na(plays$timeout_team) &
            plays$type_text != "Two-minute warning" &
            plays$type_text != "End Period"
        )
        plays <- df_slice(
          plays,
          df_first_of_groups(list(plays$qtr, plays$time, plays$ydstogo))
        )
        plays <- df_slice(
          plays,
          df_order(
            list(plays$qtr, plays$time, plays$ydstogo),
            c(FALSE, TRUE, FALSE)
          )
        )

        plays$espn_id <- df$espn
        yardline_side <- team_name_fn(split_part(plays$yardline, 1L))
        yardline_number <- as.numeric(split_part(plays$yardline, 2L))
        temp_yardline <- if_else(
          yardline_side == plays$posteam | plays$yardline_100 == 50,
          100 - yardline_number,
          yardline_number
        )
        plays$yardline_100 <- if_else(
          !is.na(temp_yardline),
          as.integer(temp_yardline),
          plays$yardline_100
        )
        plays <- df_select(
          plays,
          c(
            "game_id",
            "espn_id",
            play_id = "id",
            "desc",
            "type",
            "qtr",
            quarter_seconds_remaining = "time",
            "posteam",
            "away_team",
            "home_team",
            "yardline",
            "yardline_100",
            "ydstogo",
            "posteam_timeouts_remaining",
            "defteam_timeouts_remaining",
            "home_opening_kickoff",
            "score_differential",
            "runoff",
            "home_score",
            "away_score",
            "type_text",
            "season",
            "ot_possession"
          )
        )
      })

      if (nrow(plays) > 0) {
        plays <- df_assign(plays, list(index = seq_len(nrow(plays))))
      } else {
        plays$index <- NA_real_
      }
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    finally = {}
  )

  return(plays)
}

# ESPN's names of the teams in "<team> challenged", for timeouts charged on a
# lost challenge
challenge_teams <- c(
  "Arizona" = "ARI",
  "Atlanta" = "ATL",
  "Baltimore" = "BAL",
  "Buffalo" = "BUF",
  "Carolina" = "CAR",
  "Chicago" = "CHI",
  "Cincinnati" = "CIN",
  "Cleveland" = "CLE",
  "Dallas" = "DAL",
  "Denver" = "DEN",
  "Detroit" = "DET",
  "Green Bay" = "GB",
  "Houston" = "HOU",
  "Indianapolis" = "IND",
  "Jacksonville" = "JAX",
  "Kansas City" = "KC",
  "Los Angeles Rams" = "LA",
  "Los Angeles Chargers" = "LAC",
  "Las Vegas" = "LV",
  "Miami" = "MIA",
  "Minnesota" = "MIN",
  "New England" = "NE",
  "New Orleans" = "NO",
  "New York Giants" = "NYG",
  "New York Jets" = "NYJ",
  "Philadelphia" = "PHI",
  "Pittsburgh" = "PIT",
  "Seattle" = "SEA",
  "San Francisco" = "SF",
  "Tampa Bay" = "TB",
  "Tennessee" = "TEN",
  "Washington" = "WAS"
)

# tidyr::unnest() of the plays of the previous drives: each drive's plays, with
# the drive's team
unnest_plays <- function(drives) {
  plays <- drives$plays
  n <- vapply(plays, NROW, 1L)
  out <- df_bind_rows(plays[n > 0])
  # dplyr::bind_cols(): the class and attributes of the first
  first <- tibble::tibble(team.abbreviation = rep(drives$team.abbreviation, n))
  out <- vctrs::vec_cbind(first, out)
  df_new(df_cols(out), .row_names_info(out, 0L), first)
}

# the plays of the drive in progress, with its team
current_plays <- function(current_drive) {
  p <- current_drive[["plays"]]
  p <- df_bind_rows(if (is.data.frame(p)) list(p) else p)
  p <- tibble::as_tibble(p)
  df_assign(p, list(team.abbreviation = current_drive$team$abbreviation))
}

# dplyr::lag()
lag1 <- function(x) {
  size <- vctrs::vec_size(x)
  n <- min(1L, size)
  vctrs::vec_c(vctrs::vec_init(x, n), vctrs::vec_slice(x, seq_len(size - n)))
}

# dplyr::first(): the first value, NA when there is none
first_value <- function(x) {
  if (length(x)) x[[1L]] else x[NA_integer_]
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
