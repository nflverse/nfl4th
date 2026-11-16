season <- Sys.getenv("NFLVERSE_UPDATE_SEASON", unset = NA_character_) |>
  as.integer()
type <- Sys.getenv("NFLVERSE_UPDATE_TYPE", unset = NA_character_)
type <- rlang::arg_match0(type, c("season", "combine"))

if (type == "season") {
  # with the bootstrap summaries (see ?add_4th_probs): the draws are kept in
  # worker processes, one per core but one. The per-draw list columns are not
  # saved: load_4th_pbp(fast = TRUE) downloads every season's numbers
  nfl4th::nfl4th_start_workers()
  pbp <- nflreadr::load_pbp(season) |>
    nfl4th::add_4th_probs(bootstrap = TRUE)
  nfl4th::nfl4th_stop_workers()

  condensed <- pbp |>
    dplyr::filter(!is.na(go_boost)) |>
    dplyr::select(
      game_id,
      play_id,
      go_boost,
      go_wp,
      punt_wp,
      fg_wp,
      boot_pct,
      boot_go_pct,
      go_boost_lo,
      go_boost_hi
    )

  nflversedata::nflverse_save(
    condensed,
    file_name = paste0("pre_computed_go_boost_", season),
    nflverse_type = "nfl4th go boost",
    file_types = "rds",
    repo = "nflverse/nfl4th",
    release_tag = "nfl4th_infrastructure"
  )
} else if (type == "combine") {
  combined <- purrr::map(
    seq(2014, nflreadr::most_recent_season()),
    function(s) {
      glue::glue(
        "https://github.com/nflverse/nfl4th/releases/download/",
        "nfl4th_infrastructure/pre_computed_go_boost_{s}.rds"
      ) |>
        nflreadr::rds_from_url()
    },
    .progress = TRUE
  ) |>
    purrr::list_rbind()

  nflversedata::nflverse_save(
    combined,
    file_name = "pre_computed_go_boost",
    nflverse_type = "nfl4th go boost",
    file_types = "rds",
    repo = "nflverse/nfl4th",
    release_tag = "nfl4th_infrastructure"
  )
}
