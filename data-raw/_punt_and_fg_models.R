library(tidyverse)
library(viridis)
options(scipen = 999999)

# using more seasons for the punt model to help smooth out blocks/td returns some
seasons <- 2010:2019
pbp <- nflreadr::load_pbp(seasons) |>
  filter(play_type_nfl == "PUNT")

# thank you to Thomas Mock for the function
# https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
# maybe there's a better way to do this but calling it good enough for now
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

# figure out before and after of each punt
points <- pbp |>
  select(desc, yardline_100, kick_distance, return_yards, fumble_lost, touchback) |>
  mutate(
    # give return yards too
    yardline_after = yardline_100 - kick_distance + return_yards,
    # nflreadr fills in kick_distance on touchbacks (to the goal line), so use
    # the touchback flag too or they land at the 1 instead of the 20
    yardline_after = if_else(
      (stringr::str_detect(desc, "end zone") & is.na(kick_distance)) |
        touchback %in% 1,
      20,
      yardline_after
    ),
    # for blocked punts, just give them the ball there
    yardline_after = if_else(
      stringr::str_detect(desc, "BLOCKED") & is.na(yardline_after),
      yardline_100,
      yardline_after
    ),
    # make it in the actual field of play
    yardline_after = if_else(yardline_after > 100, 100, yardline_after),
    # there's 2 safeties that are too annoying to deal with
    yardline_after = if_else(yardline_after == 0, 1, yardline_after),
    blocked = if_else(stringr::str_detect(desc, "BLOCKED") == 1, 1, 0),
    return_td = if_else(yardline_after == 100, 1, 0),
    # there's 1 play where there was a fumble lost after a blocked punt
    # this isn't a muffed punt
    fumble_lost = if_else(blocked == 1, 0, fumble_lost)
  ) |>
  # there's like 10 of these for some reason
  filter(!is.na(yardline_after)) |>
  select(desc, yardline_100, yardline_after, blocked, return_td, fumble_lost)

points

# first, bin by yardline to get blocked and return TD pct
# another way to do this would be a smoother but these are so rare
# hopefully it doesn't matter
outliers <- points |>
  group_by(yardline_100) |>
  summarize(
    muffed = sum(fumble_lost),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = n()
  ) |>
  ungroup() |>
  mutate(
    bin = case_when(
      yardline_100 < 40 ~ 0,
      between(yardline_100, 40, 49) ~ 1,
      between(yardline_100, 50, 59) ~ 2,
      between(yardline_100, 60, 69) ~ 3,
      between(yardline_100, 70, 79) ~ 4,
      between(yardline_100, 80, 89) ~ 5,
      between(yardline_100, 90, 99) ~ 6
    )
  ) |>
  group_by(bin) |>
  mutate(
    muffed = sum(muffed),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = sum(n),
    bin_muffed_pct = muffed / n,
    bin_blocked_pct = blocked / n,
    bin_td_pct = return_td / n,
  ) |>
  ungroup()

return_tds <- outliers |>
  mutate(
    yardline_after = 100,
    density = bin_td_pct
  ) |>
  select(yardline_100, yardline_after, density) |>
  filter(density > 0)

blocks <- outliers |>
  mutate(
    # not used for anything except to pick these out later
    yardline_after = 999,
    density = bin_blocked_pct
  ) |>
  select(yardline_100, yardline_after, density) |>
  filter(density > 0)

# get density excluding blocks and returns. will add those later
density_map_normal <- points |>
  filter(blocked == 0 & return_td == 0) |>
  select(yardline_100, yardline_after) |>
  mutate(density = get_density(yardline_100, yardline_after, n = 100))

# get final percentages
df <- density_map_normal |>
  group_by(yardline_100, yardline_after) |>
  dplyr::slice(1) |>
  ungroup() |>
  arrange(yardline_100, yardline_after) |>
  group_by(yardline_100) |>
  mutate(
    tot_dens = sum(density),
    pct = density / tot_dens
  ) |>
  ungroup() |>
  bind_rows(blocks) |>
  bind_rows(return_tds) |>
  arrange(yardline_100, yardline_after) |>
  group_by(yardline_100) |>
  mutate(
    outlier_pct = sum(density * (yardline_after == 100)) +
      sum(density * (yardline_after == 999)),
    non_outlier_pct = 1 - outlier_pct,
    pct = pct * non_outlier_pct,
    pct = if_else(is.na(pct), density, pct),
    yardline_after = if_else(
      yardline_after == 999,
      yardline_100,
      yardline_after
    )
  ) |>
  ungroup() |>
  left_join(
    outliers |> select(yardline_100, bin_muffed_pct),
    by = "yardline_100"
  ) |>
  arrange(yardline_100, yardline_after) |>
  select(yardline_100, yardline_after, pct, bin_muffed_pct) |>
  filter(yardline_100 > 30)

bind_rows(
  # get a df without the return and blocked probs
  df |>
    filter(yardline_after != 100 & yardline_100 != yardline_after),
  df
) |>
  arrange(yardline_100, yardline_after) |>
  group_by(yardline_100, yardline_after) |>
  mutate(
    muff = 1:n() - 1,
    pct = if_else(muff == 1, bin_muffed_pct * pct, pct),
    pct = if_else(
      muff == 0 & yardline_after != 100 & yardline_100 != yardline_after,
      (1 - bin_muffed_pct) * pct,
      pct
    )
  ) |>
  # one last making sure all the pct add up to 1
  group_by(yardline_100) |>
  mutate(tot_pct = sum(pct), pct = pct / tot_pct) |>
  ungroup() |>
  select(
    yardline_100,
    yardline_after,
    pct,
    muff
  ) |>
  saveRDS('data-raw/punt_data.rds')


# **************************************************************************************
# field goals

# 4th downs as well as kicks: the attempt rate among 4th downs is what tells us
# how selected the long-range kicks are (see the correction below)
pbp_fourth <- nflreadr::load_pbp(2014:nflreadr::get_current_season()) |>
  filter(down == 4 | play_type_nfl == "FIELD_GOAL") |>
  mutate(
    fg_roof = case_when(roof == "outdoors" ~ 1, TRUE ~ 0),
    # same definition as the prediction path, so the two cannot drift
    fg_era = nfl4th:::get_fg_era(season),
    fg_model_roof = paste0(fg_roof, fg_era) |> as.factor(),
    fg_dist = yardline_100 + 17
  )

pbp <- pbp_fourth |>
  filter(play_type_nfl == "FIELD_GOAL", !is.na(sp), !is.na(yardline_100))

#estimate model
fg_model <- mgcv::bam(
  sp ~ s(yardline_100, by = interaction(fg_model_roof)) + fg_model_roof,
  data = pbp,
  family = "binomial"
)

save(fg_model, file = 'data-raw/fg_model.Rdata')

# ---------------------------------------------------------------------------
# long range selection correction
#
# Only teams who trust their kicker attempt very long field goals, so the
# observed make rate out there describes an elite subset rather than the
# average kicker this package has to model. At 57 yards only about 9% of teams
# facing 4th & 5 or longer even try one, so fg_model's own curve is biased
# upward from roughly 52 yards out.
#
# The fix: measure how often a field goal is attempted at each distance, fit a
# curve on the range where essentially everyone kicks, and blend the two in
# log-odds space using that attempt rate as the weight. Where everyone kicks
# the fitted curve is used unchanged; where almost nobody does, the curve from
# the unselected range takes over. The blend may only lower the probability,
# never raise it, because selection bias has a known sign.
#
# Past TAPER_FROM there are fewer than 30 attempts per roof since 2014, so
# rather than pretend to estimate it the curve is tapered to zero.
# ---------------------------------------------------------------------------

UNSELECTED_MAX <- 51 # yards; attempt rate is flat below this
TAPER_FROM <- 62 # yards; last distance with enough data to say anything
TAPER_ZERO <- 71 # yards; beyond here, call it zero

# How often is a field goal attempted, by distance? Restricted to 4th downs
# where going for it is unattractive and it is not an end-of-half heave, so the
# attempt rate reflects confidence in making the kick rather than strategy.
propensity <- pbp_fourth |>
  filter(
    down == 4,
    ydstogo >= 5,
    fg_dist >= 30,
    fg_dist <= 75,
    is.na(half_seconds_remaining) | half_seconds_remaining >= 30
  ) |>
  group_by(fg_dist) |>
  summarise(attempt_rate = mean(play_type_nfl == "FIELD_GOAL"), .groups = "drop")

# The attempt rate plateaus near 0.88 rather than 1 in the unselected range,
# because some teams go for it instead. That is strategy, not kicker doubt, so
# rescale the plateau to 1 and let the weight fall only where it really falls.
plateau <- propensity |>
  filter(fg_dist <= UNSELECTED_MAX) |>
  pull(attempt_rate) |>
  median()

propensity <- propensity |>
  mutate(weight = pmin(attempt_rate / plateau, 1))

# Fit on the unselected range only. Logit-linear rather than a spline or
# polynomial so that it extrapolates monotonically instead of curling back up.
fg_model_unselected <- stats::glm(
  sp ~ fg_dist * fg_model_roof,
  data = pbp |> filter(fg_dist <= UNSELECTED_MAX),
  family = stats::binomial()
)

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))
squeeze <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)
weight_at <- stats::approxfun(propensity$fg_dist, propensity$weight, rule = 2)

# Only compute out to TAPER_ZERO; the spline would extrapolate nonsensically
# past the data and everything beyond is zero anyway.
fg_prob_df <- tidyr::expand_grid(
  yardline_100 = 1:(TAPER_ZERO - 17),
  fg_model_roof = levels(pbp$fg_model_roof)
) |>
  mutate(
    fg_model_roof = factor(fg_model_roof, levels = levels(pbp$fg_model_roof)),
    fg_dist = yardline_100 + 17,
    weight = pmin(pmax(weight_at(fg_dist), 0), 1)
  )

fg_prob_df$p_observed <- as.numeric(
  mgcv::predict.bam(fg_model, newdata = fg_prob_df, type = "response")
)
fg_prob_df$p_unselected <- as.numeric(
  stats::predict(fg_model_unselected, newdata = fg_prob_df, type = "response")
)

fg_prob_df <- fg_prob_df |>
  mutate(
    blended = inv_logit(
      weight * logit(squeeze(p_observed)) +
        (1 - weight) * logit(squeeze(p_unselected))
    ),
    # the correction may only pull down, never up
    fg_make_prob = pmin(p_observed, blended)
  ) |>
  arrange(fg_model_roof, yardline_100) |>
  group_by(fg_model_roof) |>
  # make probability has to fall with distance
  mutate(fg_make_prob = cummin(fg_make_prob)) |>
  mutate(anchor = fg_make_prob[fg_dist == TAPER_FROM]) |>
  ungroup() |>
  mutate(
    fg_make_prob = case_when(
      fg_dist <= TAPER_FROM ~ fg_make_prob,
      fg_dist >= TAPER_ZERO ~ 0,
      TRUE ~ anchor * (TAPER_ZERO - fg_dist) / (TAPER_ZERO - TAPER_FROM)
    )
  ) |>
  select(yardline_100, fg_model_roof, fg_make_prob)

# Pad the rest of the field so the lookup in get_fg_wp() always matches.
fg_prob_df <- bind_rows(
  fg_prob_df,
  tidyr::expand_grid(
    yardline_100 = (TAPER_ZERO - 16):99,
    fg_model_roof = factor(
      levels(pbp$fg_model_roof),
      levels = levels(pbp$fg_model_roof)
    )
  ) |>
    mutate(fg_make_prob = 0)
) |>
  arrange(fg_model_roof, yardline_100)

stopifnot(
  !anyNA(fg_prob_df$fg_make_prob),
  all(fg_prob_df$fg_make_prob >= 0 & fg_prob_df$fg_make_prob <= 1),
  nrow(distinct(fg_prob_df, yardline_100, fg_model_roof)) == nrow(fg_prob_df),
  # one row per field position per group, so the lookup is a clean 1:1 join
  nrow(fg_prob_df) == 99 * nlevels(pbp$fg_model_roof)
)

saveRDS(fg_prob_df, "data-raw/fg_prob.rds")
