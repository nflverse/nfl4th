# Builds the punt model (object `punt_df` in R/sysdata.rda): for every line of
# scrimmage from the opponent's 31 back to the punting team's own 1, the
# probability of each outcome of a punt. get_punt_wp() values each outcome with
# the win probability model and takes the weighted average.
#
# Columns:
# - yardline_100: the punting team's line of scrimmage
# - yardline_after: where the receiving team takes over, as its own yard line
#   (20 for a touchback). 100 is a return touchdown.
# - muff: 1 if the punting team recovers a muff, at its yardline_100 =
#   yardline_after
# - pct: probability
#
# Three smooth parts, fit on the punts of the last 8 seasons (net punts have
# gained about 3 yards since 2010-2013, so older seasons would undervalue
# punting today):
# 1. a return touchdown (blocked punts returned for a touchdown included):
#    logistic GAM in the line of scrimmage.
# 2. a muff the punting team recovers: logistic GAM in the line of scrimmage.
#    The punting team gets the ball at the receiving team's average start.
# 3. otherwise, where the receiving team's first snap is: a discrete hazard in
#    its yard line, smooth in the line of scrimmage (a tensor product), with its
#    own term for the 20 (touchbacks). This includes blocked punts and
#    penalties on the return, because it uses the next snap rather than the
#    kick and return yards.
#
# The previous model was a 2d kernel density of 2010-2019 punts read off at the
# landing spots seen from each yard line, with block, return touchdown and muff
# rates in 10 yard bins. The average start moved by up to 2.5 yards from one
# line of scrimmage to the next, and from the opponent's 31 to 33 the touchback
# rate went 38%, 19%, 14%. This one moves smoothly: the mean |2nd difference|
# of the average start along the field is 0.02 yards instead of 0.58.
#
# Run from the package root. Takes about 2 minutes after the download.

library(dplyr)
library(mgcv)

seasons <- (nflreadr::most_recent_season() - 7):nflreadr::most_recent_season()

# ---- data -------------------------------------------------------------------
# every punt with the next snap (the receiving team's first play, or the
# punting team's after a muff). One season at a time to keep memory down.
punt_season <- function(s) {
  p <- nflreadr::load_pbp(s) |> arrange(game_id, play_id)
  snap <- !is.na(p$down) &
    !is.na(p$posteam) &
    !p$play_type_nfl %in%
      c("TIMEOUT", "END_QUARTER", "END_GAME", "KICK_OFF", "XP_KICK", "PAT2")
  idx <- which(snap)
  j <- findInterval(seq_len(nrow(p)), idx) + 1L
  nxt <- ifelse(j <= length(idx), idx[pmin(j, length(idx))], NA_integer_)
  is_punt <- p$play_type_nfl %in% "PUNT"
  k <- nxt[is_punt]
  p[is_punt, ] |>
    select(game_id, game_half, posteam, defteam, yardline_100, td_team) |>
    mutate(
      next_game = p$game_id[k],
      next_half = p$game_half[k],
      next_posteam = p$posteam[k],
      next_yardline_100 = p$yardline_100[k],
      next_play_type_nfl = p$play_type_nfl[k]
    )
}
eq <- function(a, b) dplyr::coalesce(a == b, FALSE)
punts <- purrr::map(seasons, punt_season) |>
  bind_rows() |>
  mutate(
    same_half = eq(next_game, game_id) & eq(next_half, game_half),
    outcome = case_when(
      eq(td_team, defteam) ~ "td",
      # the half ended before the next snap
      !same_half ~ NA_character_,
      eq(next_posteam, defteam) ~ "received",
      # a muff the punting team recovered (a re-kick after a penalty is not)
      eq(next_posteam, posteam) & !next_play_type_nfl %in% "PUNT" ~ "muff",
      TRUE ~ NA_character_
    ),
    y = yardline_100,
    # the receiving team's yard line at its first snap
    a = 100 - next_yardline_100
  ) |>
  filter(!is.na(outcome), !is.na(y), y >= 20)
cat("punts:", nrow(punts), "\n")
print(table(punts$outcome))

# ---- models -----------------------------------------------------------------
m_td <- gam(
  as.numeric(outcome == "td") ~ s(y, k = 4),
  data = punts,
  family = binomial(),
  method = "REML"
)
m_muff <- gam(
  as.numeric(outcome == "muff") ~ s(y, k = 4),
  data = punts |> filter(outcome != "td"),
  family = binomial(),
  method = "REML"
)
received <- punts |>
  filter(outcome == "received", !is.na(a)) |>
  mutate(a = pmin(pmax(a, 1), 99))
A <- 1:99
haz <- received |>
  group_by(y) |>
  group_modify(\(x, k) {
    n_at <- tabulate(x$a, 99)
    data.frame(a = A, ev = n_at, risk = rev(cumsum(rev(n_at))))
  }) |>
  ungroup() |>
  filter(risk > 0) |>
  mutate(tb = as.numeric(a == 20))
m_start <- bam(
  cbind(ev, risk - ev) ~ te(a, y, k = c(20, 8)) + tb + s(y, by = tb, k = 6),
  data = haz,
  family = binomial(),
  method = "fREML"
)

# ---- table --------------------------------------------------------------------
Y <- 31:99
grid <- expand.grid(a = A, y = Y) |> mutate(tb = as.numeric(a == 20))
grid$h <- as.numeric(predict(m_start, grid, type = "response"))
start <- grid |>
  group_by(y) |>
  arrange(a, .by_group = TRUE) |>
  mutate(
    h = ifelse(a == max(A), 1, h),
    q = h * lag(cumprod(1 - h), default = 1)
  ) |>
  # spots that are practically never reached are dropped
  filter(q >= 2e-4) |>
  mutate(q = q / sum(q)) |>
  ungroup()
small <- data.frame(y = Y) |>
  mutate(
    p_td = as.numeric(predict(m_td, data.frame(y = Y), type = "response")),
    p_muff = (1 - p_td) *
      as.numeric(predict(m_muff, data.frame(y = Y), type = "response"))
  )
muff_spot <- start |>
  filter(a != 20) |>
  group_by(y) |>
  summarise(a = round(sum(q * a) / sum(q)))

punt_df <- bind_rows(
  start |>
    left_join(small, by = "y") |>
    transmute(
      yardline_100 = y,
      yardline_after = a,
      pct = (1 - p_td - p_muff) * q,
      muff = 0
    ),
  small |>
    transmute(yardline_100 = y, yardline_after = 100, pct = p_td, muff = 0),
  muff_spot |>
    left_join(small, by = "y") |>
    transmute(yardline_100 = y, yardline_after = a, pct = p_muff, muff = 1)
) |>
  mutate(across(c(yardline_100, yardline_after, muff), as.numeric)) |>
  arrange(yardline_100, muff, yardline_after) |>
  tibble::as_tibble()

tot <- tapply(punt_df$pct, punt_df$yardline_100, sum)
stopifnot(all(abs(tot - 1) < 1e-9))
avg <- punt_df |>
  filter(muff == 0, yardline_after < 100) |>
  group_by(yardline_100) |>
  summarise(start = sum(pct * yardline_after) / sum(pct))
cat(
  "rows:",
  nrow(punt_df),
  "| mean |2nd difference| of the average start:",
  round(mean(abs(diff(diff(avg$start)))), 3),
  "\n"
)
saveRDS(punt_df, "data-raw/punt_data.rds")

# ---- save, keeping the other internal data ----------------------------------
# as in data-raw/_wp_model.R: only punt_df changes. data-raw/build_sysdata.R
# rebuilds everything from data-raw/punt_data.rds.
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), "punt_df")
before <- mget(untouched, envir = e)
e$punt_df <- punt_df
save(
  list = ls(e),
  file = "R/sysdata.rda",
  envir = e,
  compress = "bzip2",
  version = 2
)
e2 <- new.env()
load("R/sysdata.rda", envir = e2)
stopifnot(all(vapply(
  untouched,
  function(n) {
    identical(serialize(before[[n]], NULL), serialize(e2[[n]], NULL))
  },
  TRUE
)))
