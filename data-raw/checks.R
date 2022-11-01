library(tidyverse)

one_play <- tibble::tibble(

  # things to help find the right game (use "reg" or "post" for type)
  home_team = "GB",
  away_team = "TB",
  posteam = "GB",
  type = "post",
  season = 2020,

  # information about the situation
  qtr = 4,
  quarter_seconds_remaining = 129,
  ydstogo = 8,
  yardline_100 = 8,
  score_differential = -8,

  home_opening_kickoff = 0,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3
)

# load all

pbp <- one_play %>%
  prepare_df() %>%
  filter(down == 4)

get_punt_wp(pbp) %>%
  select(punt_wp)

get_fg_wp(pbp) %>%
  select(fg_make_prob, make_fg_wp, miss_fg_wp, fg_wp)

get_go_wp(pbp) %>%
  select(go_index, first_down_prob, wp_fail, wp_succeed, go_wp)

one_play %>%
  add_4th_probs() %>%
  nfl4th::make_table_data()

###############################################
# some checks

library(tidyverse)
yr <- 2020

new <- load_4th_pbp(yr) %>%
  select(game_id, play_id, new_go_boost = go_boost)

old <- load_4th_pbp(yr, fast = TRUE)

df <- old %>%
  left_join(new, by = c("game_id", "play_id")) %>%
  filter(!is.na(go_boost))

df %>%
  filter(ydstogo <= 3) %>%
  filter(between(go_boost, -10, 10)) %>%
  ggplot(aes(go_boost, new_go_boost)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  ggthemes::theme_fivethirtyeight() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  facet_wrap(~ydstogo)

df %>%
  filter(go_boost > 5, new_go_boost < 1) %>%
  select(game_id, play_id, score_differential, qtr, quarter_seconds_remaining, yardline_100, ydstogo, go_boost, new_go_boost)

# 0.9754796
cor(df$go_boost, df$new_go_boost)


# # # # vignette

pbp <- nfl4th::load_4th_pbp(2020:2021, fast = FALSE) %>%
  filter(down == 4)

plot <- pbp %>%
  filter(!is.na(go_wp)) %>%
  mutate(
    punt_prob = if_else(is.na(punt_wp), 0, punt_wp),
    ydstogo = ifelse(ydstogo > 10, 10, ydstogo),
    decision = case_when(
      punt_prob > fg_wp & punt_prob > go_wp ~ "Punt",
      fg_wp > punt_prob & fg_wp > go_wp ~ "Field goal",
      go_wp > punt_prob & go_wp > fg_wp ~ "Go for it",
      TRUE ~ NA_character_
    ),
    # round to nearest 5
    binned_yardline = 5 * round(yardline_100 / 5)
    ) %>%
  select(binned_yardline, yardline_100, ydstogo, go_boost, decision, vegas_wp, score_differential, qtr, posteam, home_team, spread_line)

plot_prepare <- function(df) {

  df %>%
    # for getting percent of decisions for alpha in some plots
    group_by(binned_yardline, ydstogo) %>%
    mutate(tot_n = n()) %>%
    ungroup() %>%
    group_by(binned_yardline, ydstogo, decision) %>%
    summarize(n = n(), tot_n = dplyr::first(tot_n), pct = n / tot_n) %>%
    group_by(binned_yardline, ydstogo) %>%
    arrange(binned_yardline, ydstogo, -n) %>%
    dplyr::slice(1) %>%
    # for the charts: if you've been told to punt at 4th & X
    # you should also be told to punt from that yardline at 4th & X and longer
    # a better alternative would be some sort of smoother or picking a given game
    # but ain't nobody got time for that

    # and same with FGs
    group_by(binned_yardline) %>%
    mutate(
      has_punted = cumsum(decision == "Punt"),
      has_kicked = cumsum(decision == "Field goal"),
      decision = case_when(
        has_punted > 0 ~ "Punt",
        has_kicked > 0 ~ "Field goal",
        TRUE ~ decision
      )
    ) %>%
    # if you've been told to punt on 4th & X from a given yardline
    # you should also be told to punt on 4th & X at a longer yardline
    group_by(ydstogo) %>%
    mutate(
      has_punted = cumsum(decision == "Punt"),
      decision = ifelse(has_punted > 0, "Punt", decision)
    ) %>%
    ungroup() %>%
    return()
}

plot %>%
  plot_prepare() %>%
  ggplot(aes(binned_yardline, ydstogo, fill = decision)) +
  geom_tile(aes(binned_yardline, ydstogo, width = 4.5, height = .95), alpha = 0.75) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10), expand = c(0,0)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10), expand = c(0,0)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Distance to opponent end zone",
       y = "Yards to go",
       title = "nfl4th") +
  scale_fill_brewer(palette="Dark2") +
  annotate("text",x=50, y= 2, label = "Go for it", size = 6) +
  annotate("text",x=80, y= 7, label = "Punt", size = 6) +
  annotate("text",x=20, y= 7, label = "Field goal", size = 6)

current <- pbp %>%
  filter(season == 2020) %>%
  filter(go_boost > 1.5, !is.na(go_boost), !is.na(go)) %>%
  filter(vegas_wp > .2) %>%
  group_by(posteam) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)

my_title <- glue::glue("Which teams <span style='color:red'>go for it</span> when they <span style='color:red'>should?</span> 2020")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)),
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.035) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = ggtext::element_markdown(size = 18, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "",
    y = "Go rate",
    title= my_title,
    subtitle = "Gain in win prob. at least 1.5 percentage points",
    caption = glue::glue("Sample size in parentheses\nExcl. final 30 seconds of game. Win prob >20%")
  ) +
  geom_text(data = current, aes(x = rank, y = -.015, label = glue::glue("({n})")), size = 3, show.legend = FALSE, nudge_x = 0, color="black")


# # # # # ########################################################## current season stuff

library(tidyverse)

pbp <- load_4th_pbp(nflreadr:::most_recent_season(), fast = FALSE) %>%
  filter(!is.na(go), down == 4) %>%
  mutate(
    go_boost = ifelse(go_boost > 30 & posteam == "DEN", 27, go_boost)

  )

old <- load_4th_pbp(nflreadr:::most_recent_season(), fast = TRUE) %>%
  filter(!is.na(go), down == 4) %>%
  select(game_id, play_id, old_go_boost = go_boost)

df <- pbp %>%
  left_join(old, by = c("game_id", "play_id")) %>%
  filter(!is.na(go_boost))

df %>%
  filter(ydstogo <= 3) %>%
  filter(between(go_boost, -10, 10)) %>%
  ggplot(aes(old_go_boost, go_boost)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  ggthemes::theme_fivethirtyeight() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.background = element_rect(color = "black", linetype = "solid"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  facet_wrap(~ydstogo)



# worst 10 of the season
library(gt)
pbp %>%
  filter(go == 0) %>%
  arrange(-go_boost) %>%
  mutate(rank = 1 : n()) %>%
  head(10) %>%
  select(rank, posteam, defteam, week, qtr, ydstogo, score_differential, go_boost, desc) %>%
  gt() %>%
  cols_label(
    rank = "", posteam = "Team", defteam = "Opp", week = "Week", qtr = "Qtr",
    ydstogo = "YTG", score_differential = "Diff", desc = "Play", go_boost = "WP loss"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(cells_column_labels(everything()))
  ) %>%
  text_transform(
    locations = cells_body(c(posteam, defteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>%
  cols_width(everything() ~ px(400)) %>%
  cols_width(
    c(rank) ~ px(30), c(go_boost) ~ px(80),
    c(posteam, defteam, week, score_differential, qtr, ydstogo) ~ px(50)
  ) %>%
  gtExtras::gt_theme_538() %>%
  fmt_number(columns = c(go_boost), decimals = 1) %>%
  cols_align(columns = 1:8, align = "center") %>%
  tab_header(title = paste("Worst kick decisions of", nflreadr:::most_recent_season()))


# go for it when should
library(ggpmisc)
library(ggthemes)
library(ggtext)

cutoff <- 1
num <- 3

current <- pbp %>%
  filter(go_boost >= cutoff, !is.na(go), !is.na(go_boost)) %>%
  filter(wp > .1 | (qtr == 1)) %>%
  group_by(posteam) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  filter(n >= num) %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)

my_title <- glue::glue("Which teams <span style='color:red'>go for it</span> when they <span style='color:red'>should?</span> 2022")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)),
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.045, alpha = 0.7) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    # axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.title = element_markdown(size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(size = 10, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    y = "Go rate",
    title= my_title,
    subtitle = paste("Gain in win prob. at least", cutoff, "percentage points | Win probability > 10% or 1st quarter"),
    caption = glue::glue("At least {num} opportunities | Sample size in parentheses | {lubridate::today()} | @benbbaldwin")
  ) +
  geom_text(data = current, aes(x = rank, y = -2.415, label = glue::glue("({n})")), size = 4, show.legend = FALSE, color="black")



current <- pbp %>%
  filter(go_boost >= -1.5, !is.na(go), !is.na(go_boost)) %>%
  filter(wp > .1 | (qtr == 1)) %>%
  mutate(range = ifelse(go_boost < 1.5, 0, 1)) %>%
  group_by(range, posteam) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = range, values_from = c(go, n)) %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(posteam)

current %>%
  ggplot(aes(go_0, go_1, label = posteam)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = c(0, 100)) +
  geom_vline(xintercept = mean(current$go_0)) +
  geom_hline(yintercept = mean(current$go_1)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.06, alpha = 0.8) +
  # geom_text() +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_markdown(size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(size = 10, hjust = 0.5),
    axis.title.x =  element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, max(current$go + 5))) +
  scale_y_continuous(expand = c(.03, .03), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(expand = c(.0275, .025), breaks = scales::pretty_breaks(n = 10)) +
  labs(
    y = "Go % in go situations (WP gain > 1.5)",
    x = "Go % in toss-up situations (WP gain between -1.5 and 1.5)",
    title= "2021 NFL 4th down landscape",
    caption = glue::glue("@benbbaldwin using nfl4th | {lubridate::today()}")
  ) +
  annotate("text", x = 25, y = 77, label = "By the books,\naggressive lean", color = "#008837", fontface = "bold", size = 7) +
  annotate("text", x = 4, y = 10, label = "Full of fear", color = "#7b3294", fontface = "bold", size = 7) +
  annotate("text", x = 5, y = 77, label = "By the books,\nconservative lean", color = "#7fbf7b", fontface = "bold", size = 7) +
  annotate("text", x = 20, y = 20, label = "Confused", color = "#af8dc3", fontface = "bold", size = 7)



# forfeited WP

current <- pbp %>%
  group_by(posteam) %>%
  mutate(
    games = n_distinct(game_id),
    # hard code the DEN disaster
    go_boost = ifelse(go_boost > 30 & posteam == "DEN", 27, go_boost)
  ) %>%
  ungroup() %>%
  filter(go_boost > 0, go == 0) %>%
  group_by(posteam) %>%
  summarize(
    go = sum(go_boost),
    n = n(),
    games = dplyr::first(games),
    go = go/games
  ) %>%
  ungroup() %>%
  full_join(nflfastR::teams_colors_logos %>% filter(!team_abbr %in% c("LAR", "OAK", "STL", "SD")), by=c('posteam' = 'team_abbr')) %>%
  # for teams without any wrong decisions
  mutate(go = ifelse(is.na(go), 0, go)) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)

my_title <- glue::glue("WP per game <span style='color:red'>lost by kicking in go situations</span>, 2022")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)),
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.04, alpha = 0.7) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.title = element_markdown(size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(size = 10, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, max(current$go + 5))) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    y = "Win probability lost per game",
    title= my_title,
    caption = glue::glue("@benbbaldwin using nfl4th | {lubridate::today()}")
  )







plays <- get_4th_plays("2020_20_TB_GB") %>%
  tail(1)

plays %>%
  select(desc, quarter_seconds_remaining)

plays %>%
  nfl4th::add_4th_probs() %>%
  nfl4th::make_table_data() %>%
  knitr::kable(digits = 1)
