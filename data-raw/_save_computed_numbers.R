library(tidyverse)

s <- nflfastR:::most_recent_season()

message(paste0(s))

pbp <- nfl4th::load_4th_pbp(2014:s)

condensed <- pbp %>%
  filter(!is.na(go_boost)) %>%
  select(
    game_id, play_id, go_boost
  )

condensed %>%
  saveRDS("data-raw/pre_computed_go_boost.rds")
