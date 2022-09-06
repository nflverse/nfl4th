`%>%`<-magrittr::`%>%`

s <- nflfastR:::most_recent_season()

pbp <- nfl4th::load_4th_pbp(2014:s)

condensed <- pbp %>%
  dplyr::filter(!is.na(go_boost)) %>%
  dplyr::select(
    game_id, play_id, go, go_boost, go_wp, punt_wp, fg_wp
  )

condensed %>%
  saveRDS("data-raw/pre_computed_go_boost.rds")
