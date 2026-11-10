# Builds the go for it model (object `fd_tables` in R/sysdata.rda). It gives the
# distribution of yards gained when going for it on 4th down, in four smooth
# parts:
#
# 1. conversion: probability of gaining the line to gain (or a first down by
#    penalty). Logistic GAM in distance and field position with a 4th down
#    effect, team strength (spread and total), the era (2001-2019 and 2020+,
#    see get_fd_era(); the latest era covers future seasons) and a late game
#    term (offenses that trail late convert more often against defenses that
#    play soft, so those attempts don't set the rate for the rest of the
#    game).
# 2. touchdown given a conversion that isn't goal to go, by the room left
#    between the line to gain and the end zone.
# 3. where a conversion that doesn't score ends: yards past the line to gain,
#    as a discrete hazard in those yards by distance, room and down. Small
#    gains (0 to 8 yards past the line) get their own terms because the
#    distribution has spikes there.
# 4. where a failed attempt ends: 4th down counts by distance, shrunk toward
#    3rd and 4th downs combined.
#
# The models are fit on 3rd and 4th down runs, passes and sacks (and defensive
# penalties that give a first down) from 2001 on, then tabulated for 4th down
# so predictions are table lookups (see R/go_model.R).
#
# The old model (a 76 class xgboost classifier, 2014-2019) is replaced because
# its conversion probability jumped from one yard line to the next and it
# underestimated conversion on recent 4th downs (52% predicted vs 57% actual in
# 2020-2026).
#
# Run from the package root. Takes about a minute after the download.
# Set FD_FUNCTIONS_ONLY <- TRUE before sourcing to only define the functions.

library(dplyr)
library(mgcv)
# get_fd_era() and fd_desperate() are shared with the prediction code
source("R/helpers.R")
source("R/go_model.R")

fd_t_max <- 30L # distances beyond this use 30
fd_r_max <- 40L # room past the line to gain beyond this uses 40
fd_fail_t_max <- 20L

fd_model_data <- function(pbp) {
  pbp |>
    filter(
      season >= 2001,
      down %in% c(3, 4),
      qb_kneel == 0,
      rush == 1 | pass == 1,
      !is.na(posteam),
      !is.na(yardline_100),
      !is.na(spread_line),
      ydstogo >= 1,
      play_type_nfl %in% c("RUSH", "PASS", "SACK") | first_down_penalty %in% 1
    ) |>
    mutate(
      # a defensive penalty that gives a first down counts as gaining the
      # distance, or the penalty yards if they are more
      yards_gained = case_when(
        first_down_penalty %in% 1 & penalty_yards < ydstogo ~ as.numeric(
          ydstogo
        ),
        first_down_penalty %in% 1 & penalty_yards >= ydstogo ~ as.numeric(
          penalty_yards
        ),
        TRUE ~ as.numeric(yards_gained)
      ),
      gain = pmin(pmax(yards_gained, -10), 65),
      conv = as.numeric(yards_gained >= ydstogo),
      td = as.numeric(yards_gained >= yardline_100),
      gtg = as.numeric(ydstogo >= yardline_100),
      posteam_spread = if_else(posteam == home_team, spread_line, -spread_line),
      f4 = factor(as.numeric(down == 4), levels = c(0, 1)),
      lt = log(ydstogo),
      yl = yardline_100,
      fgtg = factor(gtg, levels = c(0, 1)),
      fera = factor(get_fd_era(season), levels = c(0, 1)),
      r = yardline_100 - ydstogo,
      desp = fd_desperate(game_seconds_remaining, score_differential)
    ) |>
    filter(!is.na(gain))
}

fd_fit <- function(tr, nthreads = parallel::detectCores()) {
  # 1. conversion. The era term is by down; with training data from one era
  # only (e.g. a holdout fit) it is left out
  eras <- sort(unique(as.integer(as.character(tr$fera))))
  tr$fera <- factor(tr$fera, levels = as.character(eras))
  f_conv <- conv ~ f4 +
    s(lt, k = 10) +
    s(lt, by = f4, k = 6) +
    s(yl, k = 10) +
    ti(lt, yl, k = c(5, 5)) +
    fgtg +
    s(yl, by = fgtg, k = 5) +
    posteam_spread +
    total_line +
    desp +
    f4:fera
  if (length(eras) == 1) {
    f_conv <- stats::update(f_conv, . ~ . - f4:fera)
  }
  m_conv <- bam(
    f_conv,
    data = tr,
    family = binomial(),
    discrete = TRUE,
    nthreads = nthreads
  )
  # 2. touchdown given a conversion that isn't goal to go
  cv <- tr |> filter(conv == 1, gtg == 0) |> mutate(lr = log(r))
  m_td <- bam(
    td ~ f4 +
      s(lr, k = 8) +
      s(lt, k = 6) +
      ti(lr, lt, k = c(5, 5)) +
      posteam_spread +
      total_line,
    data = cv,
    family = binomial(),
    discrete = TRUE,
    nthreads = nthreads
  )
  # 3. yards past the line to gain for conversions that don't score. Discrete
  # hazard; touchdowns and 65+ yard gains are censored
  cv <- cv |>
    mutate(
      tt = pmin(ydstogo, fd_t_max),
      rb = pmin(r, fd_r_max),
      ff4 = f4,
      cens = td == 1 | yards_gained >= 65,
      e_obs = ifelse(cens, pmin(r, 65 - ydstogo), yards_gained - ydstogo)
    )
  E <- 0:60
  agg <- cv |>
    group_by(tt, rb, ff4) |>
    group_modify(function(x, k) {
      data.frame(
        e = E,
        risk = vapply(E, \(e) sum(x$e_obs > e | (!x$cens & x$e_obs == e)), 1),
        ev = vapply(E, \(e) sum(!x$cens & x$e_obs == e), 1)
      )
    }) |>
    ungroup() |>
    filter(risk > 0) |>
    mutate(lt = log(tt), lr = log(rb), fe = factor(pmin(e, 8)))
  m_haz <- bam(
    cbind(ev, risk - ev) ~ fe +
      s(e, k = 15) +
      s(lt, k = 6) +
      ti(e, lt, k = c(8, 5)) +
      s(lt, by = fe, k = 4) +
      s(lr, k = 6) +
      ti(e, lr, k = c(8, 5)) +
      ff4,
    data = agg,
    family = binomial(),
    nthreads = nthreads
  )
  # 4. failed attempts: where the play ends, 4th down counts by distance
  # shrunk toward 3rd and 4th downs combined
  fl <- tr |>
    filter(conv == 0, gtg == 0) |>
    mutate(tt = pmin(ydstogo, fd_fail_t_max), k = pmax(gain, -10))
  fail <- lapply(seq_len(fd_fail_t_max), function(t) {
    ks <- -10:(t - 1)
    a <- fl$k[fl$tt == t]
    pa <- tabulate(match(a, ks), nbins = length(ks)) + 0.5
    pa <- pa / sum(pa)
    n4 <- tabulate(
      match(fl$k[fl$tt == t & fl$down == 4], ks),
      nbins = length(ks)
    )
    stats::setNames((n4 + 100 * pa) / (sum(n4) + 100), ks)
  })
  list(
    m_conv = m_conv,
    m_td = m_td,
    m_haz = m_haz,
    haz_levels = levels(agg$fe),
    fail = fail,
    eras = eras
  )
}

# Tabulate the fit for 4th down: logits at spread 0, total 45 and the latest
# era, plus the spread, total and late game coefficients and the shift of each
# era (eras the training data doesn't cover use the latest era it does)
fd_tabulate <- function(fit, all_eras = 0:1) {
  ref <- max(fit$eras)
  base <- function(d) {
    d |>
      mutate(
        f4 = factor(1, levels = c(0, 1)),
        ff4 = f4,
        lt = log(ydstogo),
        yl = yardline_100,
        posteam_spread = 0,
        total_line = 45,
        desp = 0,
        fera = factor(ref, levels = as.character(fit$eras))
      )
  }
  era_effect <- function(m, nd) {
    x <- nd[rep(1, length(all_eras)), ]
    e <- pmin(all_eras, ref)
    e <- vapply(e, \(v) max(fit$eras[fit$eras <= v], min(fit$eras)), 1)
    x$fera <- factor(e, levels = as.character(fit$eras))
    stats::setNames(
      round(as.numeric(predict(m, x) - predict(m, nd[1, ])), 6),
      all_eras
    )
  }
  # 1. conversion [distance, yardline]
  g <- expand.grid(ydstogo = seq_len(fd_t_max), yardline_100 = 1:99) |>
    filter(yardline_100 >= ydstogo) |>
    mutate(gtg = as.numeric(ydstogo == yardline_100)) |>
    base() |>
    mutate(fgtg = factor(gtg, levels = c(0, 1)))
  conv <- matrix(NA_real_, fd_t_max, 99)
  conv[cbind(g$ydstogo, g$yardline_100)] <- predict(fit$m_conv, g)
  # 2. touchdown [distance, room]
  gt <- expand.grid(ydstogo = seq_len(fd_t_max), r = 1:98) |>
    mutate(yardline_100 = ydstogo + r) |>
    filter(yardline_100 <= 99) |>
    base() |>
    mutate(lr = log(r))
  td <- matrix(NA_real_, fd_t_max, 98)
  td[cbind(gt$ydstogo, gt$r)] <- predict(fit$m_td, gt)
  # 3. yards past the line to gain: hazard [distance, room, yards 0..60]. The
  # hazard past 60 yards is the one at 60 (see R/go_model.R)
  hazard <- array(NA_real_, c(fd_t_max, fd_r_max, 61))
  for (t in seq_len(fd_t_max)) {
    for (rb in seq_len(fd_r_max)) {
      nd <- data.frame(
        e = 0:60,
        lt = log(t),
        lr = log(rb),
        ff4 = factor(1, levels = c(0, 1))
      )
      nd$fe <- factor(pmin(nd$e, 8), levels = fit$haz_levels)
      hazard[t, rb, ] <- stats::plogis(predict(fit$m_haz, nd))
    }
  }
  cf <- function(m, terms = c("posteam_spread", "total_line")) {
    round(stats::coef(m)[terms], 6)
  }
  list(
    conv = round(conv, 6),
    conv_coef = cf(fit$m_conv, c("posteam_spread", "total_line", "desp")),
    conv_era = era_effect(fit$m_conv, g),
    td = round(td, 6),
    td_coef = cf(fit$m_td),
    hazard = signif(hazard, 4),
    fail = lapply(fit$fail, \(p) signif(p, 6)),
    ref_total = 45
  )
}

if (!isTRUE(get0("FD_FUNCTIONS_ONLY", ifnotfound = FALSE))) {
  cols <- c(
    "season",
    "down",
    "ydstogo",
    "yardline_100",
    "posteam",
    "home_team",
    "spread_line",
    "total_line",
    "qb_kneel",
    "rush",
    "pass",
    "play_type_nfl",
    "first_down_penalty",
    "penalty_yards",
    "yards_gained",
    "game_seconds_remaining",
    "score_differential"
  )
  pbp <- purrr::map(
    2001:nflreadr::most_recent_season(),
    \(s) nflreadr::load_pbp(s) |> select(all_of(cols))
  ) |>
    bind_rows()
  model_data <- fd_model_data(pbp)
  cat(
    "training rows:",
    nrow(model_data),
    " 4th downs:",
    sum(model_data$down == 4),
    "\n"
  )
  fit <- fd_fit(model_data)
  fd_tables <- fd_tabulate(fit)
  saveRDS(fd_tables, "data-raw/fd_tables.rds")

  # R/sysdata.rda holds other objects. Load them and write them back untouched
  # so that only this model changes (see data-raw/_wp_model.R)
  e <- new.env()
  load("R/sysdata.rda", envir = e)
  untouched <- setdiff(ls(e), "fd_tables")
  before <- mget(untouched, envir = e)
  e$fd_tables <- fd_tables
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
}
