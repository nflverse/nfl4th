# Builds the go for it model (object `fd_tables` in R/sysdata.rda). It gives the
# distribution of yards gained when going for it on 4th down, in four smooth
# parts:
#
# 1. conversion: probability of gaining the line to gain (or a first down by
#    penalty). Logistic GAM in distance and field position with a 4th down
#    effect, team strength (spread and total) and the era (2001-2019 and
#    2020+, see get_fd_era(); the latest era covers future seasons). The era
#    shift applies to 4th downs and is a smooth in distance (see
#    fd_era_vars()), and on 4th downs the spread's effect changes with distance
#    (see fd_spread_vars()). Late and desperate 4th downs get their own
#    smooth in distance, which predictions use for late plays (see
#    fd_late_vars() and fd_is_late()).
# 2. touchdown given a conversion that isn't goal to go, by the room left
#    between the line to gain and the end zone.
# 3. where a conversion that doesn't score ends: yards past the line to gain,
#    as a discrete hazard in those yards by distance, room and down. Small
#    gains (0 to 8 yards past the line) get their own terms because the
#    distribution has spikes there.
# 4. where a failed attempt ends: 4th down counts by distance, shrunk toward
#    3rd and 4th downs combined.
#
# The models are fit on 3rd and 4th down runs, passes and sacks (plus defensive
# penalties that give a first down, and turnovers as failed attempts, see
# fd_model_data()) from 2001 on, then tabulated for 4th down
# so predictions are table lookups (see R/go_model.R). 3rd downs help: fit on
# 4th downs only, season grouped CV log loss on 4th downs is worse (+0.0006);
# leaving out 3rd downs since 2020, when teams going for it more often may have
# made 3rd downs less all-or-nothing, makes no difference, and neither does
# down-weighting 3rd downs (weight 0.4). The 4th down era term carries
# the larger 3rd to 4th down gap since 2020.
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
# get_fd_era() is shared with the prediction code
source("R/helpers.R")
source("R/go_model.R")

fd_t_max <- 30L # distances beyond this use 30
fd_r_max <- 40L # room past the line to gain beyond this uses 40
fd_fail_t_max <- 20L

# Training data: 3rd and 4th down runs, passes and sacks, defensive penalties
# that give a first down, and interceptions and lost fumbles. A turnover is a
# failed attempt: the other team takes over where the play ended (for an
# interception, the spot of the catch less the return), counted at most one
# yard short of the line to gain. Leaving turnovers out (as an earlier version
# did) dropped 4% of late 4th down attempts, almost all failures, and made 4th
# and 8+ look about 2.5 points easier to convert.
#
# Defensive penalties that give a first down count (pass interference, holding,
# offside, roughing the passer), also when the play itself isn't recorded as a
# run or pass (filtering on rush or pass, as the old model did, dropped 40 4th
# downs and 161 3rd downs, mostly offside). Encroachment and neutral zone
# infractions don't count and are left out: the snap never happened, and the
# offense may only have been trying to draw the defense offside before
# punting or kicking (120 4th downs and 413 3rd downs). Pre-snap fouls by the
# offense (a false start, delay of game) replay the down and are left out, as
# are plays wiped out by other penalties. Fouls against a punt or field goal
# formation (roughing the kicker, for example) belong to the kick, and fake
# punts and field goals are left out.
fd_kick_formation <- function(desc, play_type_nfl) {
  play_type_nfl %in%
    c("PUNT", "FIELD_GOAL") |
    grepl("Punt formation|Field Goal formation|punts|field goal", desc)
}

# defensive fouls the offense can draw before the snap with a hard count
fd_drawn_fouls <- c("Encroachment", "Neutral Zone Infraction")

fd_model_data <- function(pbp) {
  pbp |>
    filter(
      season >= 2001,
      down %in% c(3, 4),
      qb_kneel == 0,
      !qb_spike %in% 1,
      !is.na(posteam),
      !is.na(yardline_100),
      !is.na(spread_line),
      ydstogo >= 1,
      ((rush == 1 | pass == 1) &
        play_type_nfl %in%
          c(
            "RUSH",
            "PASS",
            "SACK",
            "INTERCEPTION",
            "FUMBLE_RECOVERED_BY_OPPONENT"
          )) |
        (first_down_penalty %in%
          1 &
          !fd_kick_formation(desc, play_type_nfl) &
          !penalty_type %in% fd_drawn_fouls)
    ) |>
    mutate(
      turnover = !first_down_penalty %in% 1 &
        (interception %in% 1 | fumble_lost %in% 1),
      # a defensive penalty that gives a first down counts as gaining the
      # distance, or the penalty yards if they are more
      yards_gained = case_when(
        first_down_penalty %in% 1 & penalty_yards < ydstogo ~ as.numeric(
          ydstogo
        ),
        first_down_penalty %in% 1 & penalty_yards >= ydstogo ~ as.numeric(
          penalty_yards
        ),
        interception %in% 1 ~ pmin(
          dplyr::coalesce(as.numeric(air_yards), 0) -
            dplyr::coalesce(as.numeric(return_yards), 0),
          ydstogo - 1
        ),
        turnover ~ pmin(
          dplyr::coalesce(as.numeric(yards_gained), 0),
          ydstogo - 1
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
      r = yardline_100 - ydstogo
    ) |>
    filter(!is.na(gain))
}

# Era indicators for the conversion model: one per later era, 1 on that era's
# 4th downs. Each gets its own smooth in log distance, so the era shift can
# differ between short and long distances. The smooths aren't centered (numeric
# by variables), so they also carry the level of the shift. Since 2020 4th down
# conversion is up at short and medium distances and slightly down at long
# ones. In season grouped CV a single shift per down was 4.4 points too high on
# 2020-2026 4th and 8+ (1.5 with the smooth shift). No era adjustment, an
# earlier boundary (2014 or 2017, about half the sample) and a shift on 4th and
# short only were all worse, mostly at long distances. 3rd downs get no era
# term: it made no difference to 4th downs (log loss within 0.00001).
fd_era_names <- function(eras) {
  if (length(eras) < 2) {
    return(character())
  }
  paste0("era", eras[-1], "_d4")
}
fd_era_vars <- function(d, eras) {
  for (e in eras[-1]) {
    d[[paste0("era", e, "_d4")]] <- as.numeric(
      as.character(d$fera) == as.character(e) & d$f4 == "1"
    )
  }
  d
}

# 4th down spread terms: an extra spread slope on 4th downs that changes
# linearly in log distance (centered at 4th and 5). The spread matters less on
# 4th and long: the slope is about 0.017 at 4th and 1 (as on 3rd downs), 0.011
# at 4th and 5 and 0.009 at 4th and 10. With one shared slope, 7+ point
# favorites converted 4.6 points less often than predicted on 4th and 6+
# (2001-2026, season grouped CV; 2.9 with these terms). Log loss can't tell the
# two apart once the late terms below are in, which carry much of what this
# term first picked up (95% of 4th and 6+ attempts by 7+ point underdogs are
# late, 52% of those by 7+ point favorites). A constant
# 4th down slope barely moved and a 4th down total slope made no difference.
fd_spread_ref <- log(5)
fd_spread_vars <- function(d) {
  is4 <- as.numeric(d$f4 == "1")
  d$sp4 <- d$posteam_spread * is4
  d$sp4lt <- d$posteam_spread * is4 * (d$lt - fd_spread_ref)
  d
}

# Late and desperate situations (see fd_is_late() in R/go_model.R; here with the
# play's vegas_wp). Most 4th and long attempts come late in a game the offense
# is losing (2015-2026, 4th and 8-10: 527 of 665 in the last 10 minutes of the
# 4th quarter while trailing), against defenses that give up more underneath,
# and they convert more often than 4th and long in a normal situation
# (2001-2026: 36.1% vs 27.6% at 8-10 yards, 23.3% vs 17.5% at 11+). With one
# curve for both, normal 4th and long was predicted at the late rate (season
# grouped CV, not goal to go: 8-10 35.8% vs 27.6% actual, z -2.5; 11+ 23.3% vs
# 17.5%, z -1.8). Late 3rd downs get a constant and late 4th downs a smooth in
# distance, and the tables carry the late 4th down shift by distance, which
# predictions add for late plays. In the same CV both normal and late 4th downs
# are then calibrated at every distance (|z| <= 1.0) and log loss on 4th downs
# improves (-0.0005, normal ones -0.0009). The last 2 minutes of the 1st half
# aren't late: counting them gave about the same log loss but overpredicted
# those 4th downs, which convert like normal ones or worse (see fd_is_late()).
fd_late_vars <- function(d) {
  late <- fd_is_late(
    d$qtr,
    d$score_differential,
    d$game_seconds_remaining,
    d$vegas_wp
  )
  d$late3 <- as.numeric(late & d$f4 == "0")
  d$late4 <- as.numeric(late & d$f4 == "1")
  d
}

fd_fit <- function(tr, nthreads = parallel::detectCores()) {
  # 1. conversion. With training data from one era only (e.g. a holdout fit)
  # the era terms are left out
  eras <- sort(unique(as.integer(as.character(tr$fera))))
  tr$fera <- factor(tr$fera, levels = as.character(eras))
  tr <- fd_era_vars(tr, eras) |> fd_spread_vars() |> fd_late_vars()
  f_conv <- conv ~ f4 +
    s(lt, k = 10) +
    s(lt, by = f4, k = 6) +
    s(yl, k = 10) +
    ti(lt, yl, k = c(5, 5)) +
    fgtg +
    s(yl, by = fgtg, k = 5) +
    posteam_spread +
    sp4 +
    sp4lt +
    total_line +
    late3 +
    s(lt, by = late4, k = 5)
  if (length(eras) > 1) {
    era_terms <- sprintf("s(lt, by = %s, k = 5)", fd_era_names(eras))
    f_conv <- stats::update(
      f_conv,
      stats::as.formula(paste(". ~ . +", paste(era_terms, collapse = " + ")))
    )
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
    # like the other parts. Without discrete = TRUE, bam warns that its inner
    # iterations didn't converge on this data; where there is data the two fits
    # agree to within 0.005 on the hazard
    discrete = TRUE,
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

# Tabulate the fit for 4th down in a normal situation (late terms 0): logits at
# spread 0, total 45 and the latest era, plus the total coefficient, the spread
# slope by distance, the shift of each era by distance (eras the training data
# doesn't cover use the latest era it does) and the late shift by distance
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
        fera = factor(ref, levels = as.character(fit$eras))
      ) |>
      fd_era_vars(fit$eras) |>
      fd_spread_vars() |>
      mutate(late3 = 0, late4 = 0)
  }
  # conversion logit of each era minus the latest one [distance, era]. The era
  # terms are smooths in distance only, so the shift doesn't depend on field
  # position
  era_effect <- function(m) {
    nd <- data.frame(ydstogo = seq_len(fd_t_max), yardline_100 = 75) |>
      base() |>
      mutate(fgtg = factor(0, levels = c(0, 1)))
    ref_lp <- predict(m, nd)
    out <- vapply(
      all_eras,
      function(v) {
        e <- max(fit$eras[fit$eras <= min(v, ref)], min(fit$eras))
        x <- nd
        x$fera <- factor(e, levels = as.character(fit$eras))
        x <- fd_era_vars(x, fit$eras)
        round(as.numeric(predict(m, x) - ref_lp), 6)
      },
      numeric(fd_t_max)
    )
    colnames(out) <- all_eras
    out
  }
  # conversion logit of a late 4th down minus a normal one [distance]. The late
  # term is a smooth in distance only
  late_effect <- function(m) {
    nd <- data.frame(ydstogo = seq_len(fd_t_max), yardline_100 = 75) |>
      base() |>
      mutate(fgtg = factor(0, levels = c(0, 1)))
    lt <- nd
    lt$late4 <- 1
    round(as.numeric(predict(m, lt) - predict(m, nd)), 6)
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
  # spread slope of the 4th down conversion logit [distance]
  b <- stats::coef(fit$m_conv)
  conv_spread <- round(
    b[["posteam_spread"]] +
      b[["sp4"]] +
      b[["sp4lt"]] * (log(seq_len(fd_t_max)) - fd_spread_ref),
    6
  )
  list(
    conv = round(conv, 6),
    conv_coef = cf(fit$m_conv, "total_line"),
    conv_spread = conv_spread,
    conv_era = era_effect(fit$m_conv),
    conv_late = late_effect(fit$m_conv),
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
    "qb_spike",
    "rush",
    "pass",
    "play_type_nfl",
    "desc",
    "first_down_penalty",
    "penalty_type",
    "penalty_yards",
    "yards_gained",
    "interception",
    "fumble_lost",
    "air_yards",
    "return_yards",
    # game situation, for the late terms (see fd_late_vars())
    "qtr",
    "game_seconds_remaining",
    "score_differential",
    "vegas_wp"
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
