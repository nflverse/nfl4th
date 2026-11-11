# Field goal model. The punt model is built in data-raw/_punt_model.R.
library(tidyverse)
options(scipen = 999999)

# ******************************************************************************
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

# A distance counts as "unselected" while the attempt rate is still this share
# of the group's own plateau. Indoors that holds two to three yards further out
# than outdoors, which is why the range is derived per group rather than fixed.
WEIGHT_FLOOR <- 0.80
TAPER_FROM <- 62 # yards; last distance with enough data to say anything
TAPER_ZERO <- 71 # yards; beyond here, call it zero
PLATEAU_MAX <- 51 # yards; every group is still unselected below this

# How often is a field goal attempted, by distance and by roof/era group?
# Restricted to 4th downs where going for it is unattractive and it is not an
# end-of-half heave, so the attempt rate reflects confidence in making the kick
# rather than strategy. Smoothed over distance per group, because the raw
# per-distance cells are thin once split four ways.
fourth_downs <- pbp_fourth |>
  filter(
    down == 4,
    ydstogo >= 5,
    fg_dist >= 30,
    fg_dist <= 75,
    is.na(half_seconds_remaining) | half_seconds_remaining >= 30
  ) |>
  mutate(attempted = play_type_nfl == "FIELD_GOAL")

attempt_model <- mgcv::bam(
  attempted ~ s(fg_dist, by = interaction(fg_model_roof)) + fg_model_roof,
  data = fourth_downs,
  family = "binomial"
)

propensity <- tidyr::expand_grid(
  fg_dist = 30:75,
  fg_model_roof = levels(pbp$fg_model_roof)
) |>
  mutate(
    fg_model_roof = factor(fg_model_roof, levels = levels(pbp$fg_model_roof))
  )
propensity$attempt_rate <- as.numeric(
  mgcv::predict.bam(attempt_model, newdata = propensity, type = "response")
)

# The attempt rate plateaus below 1 even at short range, because some teams go
# for it instead. That is strategy, not kicker doubt, so rescale each group
# against its own plateau and let the weight fall only where it really falls.
propensity <- propensity |>
  group_by(fg_model_roof) |>
  mutate(
    plateau = stats::median(attempt_rate[fg_dist <= PLATEAU_MAX]),
    weight = pmin(attempt_rate / plateau, 1)
  ) |>
  ungroup()

unselected_max <- propensity |>
  filter(weight >= WEIGHT_FLOOR) |>
  group_by(fg_model_roof) |>
  summarise(max_dist = max(fg_dist), .groups = "drop")

print(unselected_max)

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))
squeeze <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)

# Only compute out to TAPER_ZERO; the spline would extrapolate nonsensically
# past the data and everything beyond is zero anyway.
fg_prob_df <- tidyr::expand_grid(
  yardline_100 = 1:(TAPER_ZERO - 17),
  fg_model_roof = levels(pbp$fg_model_roof)
) |>
  mutate(
    fg_model_roof = factor(fg_model_roof, levels = levels(pbp$fg_model_roof)),
    fg_dist = yardline_100 + 17
  ) |>
  left_join(
    propensity |> select(fg_dist, fg_model_roof, weight),
    by = c("fg_dist", "fg_model_roof")
  ) |>
  mutate(weight = pmin(pmax(dplyr::coalesce(weight, 1), 0), 1))

fg_prob_df$p_observed <- as.numeric(
  mgcv::predict.bam(fg_model, newdata = fg_prob_df, type = "response")
)

# Curve from the unselected range only, fit per group on that group's own
# range. Logit-linear rather than a spline or polynomial so that it
# extrapolates monotonically instead of curling back up.
fg_prob_df$p_unselected <- NA_real_
for (group in levels(pbp$fg_model_roof)) {
  limit <- unselected_max$max_dist[unselected_max$fg_model_roof == group]
  unselected_fit <- stats::glm(
    sp ~ fg_dist,
    data = pbp |> filter(fg_model_roof == group, fg_dist <= limit),
    family = stats::binomial()
  )
  rows <- fg_prob_df$fg_model_roof == group
  fg_prob_df$p_unselected[rows] <- as.numeric(
    stats::predict(
      unselected_fit,
      newdata = fg_prob_df[rows, ],
      type = "response"
    )
  )
}

fg_prob_df <- fg_prob_df |>
  mutate(
    blended = inv_logit(
      weight *
        logit(squeeze(p_observed)) +
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
  nrow(fg_prob_df) == 99 * nlevels(pbp$fg_model_roof),
  # make probability must fall with distance within every group
  all(
    fg_prob_df |>
      group_by(fg_model_roof) |>
      summarise(ok = all(diff(fg_make_prob) <= 1e-12), .groups = "drop") |>
      pull(ok)
  )
)

saveRDS(fg_prob_df, "data-raw/fg_prob.rds")
