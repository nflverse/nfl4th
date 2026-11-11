# Win probability of the 1st down states that follow a 4th down decision
#
# Every state nfl4th values after a decision is a 1st down: 1st and 10 (or
# goal) for the offense after a conversion, for the other team after a turnover
# on downs, a punt or a missed field goal, and after the kickoff that follows a
# score. In regulation these are valued with nfl4th's own model, trained on
# regulation 1st and 10 and 1st and goal snaps. It follows the recipe of
# nflfastR's vegas_wp model:
#
# 1. offset: a probit model of the final margin in the spirit of Stern (1994).
#    Every term is divided by the remaining-time scale of the margin,
#    sigma * ((t + eps) / 3600) ^ alpha. The score differential, spread and
#    home terms are weighted by `shrink` so the monotone trees can correct
#    their slope in the direction they are allowed to.
#    At the end of a half a possession is worth the field goals still in play:
#    the possession terms fade, by the time needed to get into field goal
#    range, toward the value of a field goal from the current spot, minus the
#    other team's chance of a field goal if it gets the ball back in time.
# 2. fade(t): the whole offset fades over the last minutes so the trees can
#    learn the end game staircase in score differential.
# 3. trees: xgboost trained on top of the offset with monotone constraints. The
#    number of trees used depends on the clock: few early in the game, where
#    labels are noisy, and many in the last minutes. Margins are interpolated
#    between knots so WP is continuous in time.
# 4. A team can't be worse off after scoring a touchdown on this snap: with more
#    than five minutes left, WP is capped at the value of a TD + PAT.
# 5. A team can't be worse off than if it kicked a field goal on this snap: WP
#    is at least the value of the attempt (made: 3 points and a kickoff, missed:
#    the other team takes over at the spot of the kick). A kick at the end of
#    the game is valued by the final result, a tie by nflfastR's overtime model.
# 6. The end of the game, as in nflfastR's vegas_wp (nflfastR#625): in the last
#    4 minutes WP is averaged over +-16 seconds and +-2 yards; a team ahead
#    that can kneel out the clock is at 0.9998, moving back to the model 10 to
#    40 seconds short of that; a team behind deep in its own half moves to 0
#    over the last 20 seconds. (nflfastR's rule for 4th down kicks in the last
#    30 seconds doesn't apply: every state here is a 1st down.)
#
# In regulation nfl4th averages this model with nflfastR's vegas_wp. Overtime
# states are valued with nflfastR's overtime model (see R/overtime.R).
#
# Built by data-raw/_wp_model.R and stored in R/sysdata.rda as
# `wp_first_down_model` (list with the xgboost model as raw UBJSON, the probit
# coefficients and the constants).

wpfd_features <- c(
  "r2h_first_half",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "score_differential",
  "posteam_spread",
  "stern_z",
  "spread_z",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# terms of the probit offset weighted by `shrink`
wpfd_score_terms <- c("sd", "spread_t", "home_t")

# remaining-time scale of the final margin
wpfd_tscale <- function(g, k) {
  k$sigma * ((g + k$eps) / 3600)^k$alpha
}

# feature matrix for the trees. `x` needs game_seconds_remaining,
# half_seconds_remaining, score_differential, posteam_spread, home,
# receive_2h_ko, yardline_100 and both timeouts columns
wpfd_tree_matrix <- function(x, k) {
  g <- x$game_seconds_remaining
  s <- wpfd_tscale(g, k)
  spread_rem <- x$posteam_spread * g / 3600
  X <- cbind(
    r2h_first_half = x$receive_2h_ko * as.numeric(g > 1800),
    home = x$home,
    half_seconds_remaining = x$half_seconds_remaining,
    game_seconds_remaining = g,
    score_differential = x$score_differential,
    posteam_spread = x$posteam_spread,
    stern_z = (x$score_differential + spread_rem) / s,
    spread_z = spread_rem / s,
    yardline_100 = x$yardline_100,
    posteam_timeouts_remaining = x$posteam_timeouts_remaining,
    defteam_timeouts_remaining = x$defteam_timeouts_remaining
  )
  storage.mode(X) <- "double"
  X
}

# columns of the probit offset (all divided by the time scale)
wpfd_design <- function(x, k) {
  g <- x$game_seconds_remaining
  h <- x$half_seconds_remaining
  s <- wpfd_tscale(g, k)
  tau <- g / 3600
  yl <- x$yardline_100
  # The possession terms fade when the half runs out, by the time needed to get
  # into field goal range (scoring before halftime from the opponent's 35 or
  # closer only needs the time to kick)...
  left <- pmin(h / wpfd_fg_range_time(yl), 1)
  # ... toward the value of a field goal from here
  fg_eoh <- (1 - left) * 3 * wpfd_fg_make(yl)
  # The other team's chance of a field goal before halftime: it takes over
  # after a punt and needs the time to get into range. On 1st down this
  # possession ends after three more plays. The defense stops the clock after
  # each play while it has timeouts; without timeouts a play takes about 40
  # seconds.
  opp_start <- pmin(140 - yl, 80)
  dto <- x$defteam_timeouts_remaining
  t_end <- 5 + 6 * pmin(3, dto) + 40 * pmax(3 - dto, 0)
  opp_reach <- (1 - left) *
    pmin(pmax(h - t_end, 0) / wpfd_fg_range_time(opp_start), 1)
  # timeouts and the clock matter at the end of either half. In the 2nd half
  # half seconds and game seconds are the same
  late <- exp(-g / 300)
  late1 <- exp(-h / 300) * as.numeric(g > 1800)
  to_diff <- x$posteam_timeouts_remaining - x$defteam_timeouts_remaining
  fp <- (50 - yl) / 50
  Z <- cbind(
    sd = x$score_differential,
    spread_t = x$posteam_spread * tau,
    home_t = x$home * tau,
    const = left,
    field = fp * left,
    field2 = fp^2 * left,
    field3 = fp^3 * left,
    r2h = x$receive_2h_ko * as.numeric(g > 1800),
    to_diff_late = to_diff * late,
    const_late = late,
    to_diff_late1 = to_diff * late1,
    const_late1 = late1,
    fg_eoh = fg_eoh,
    opp_eoh = opp_reach
  )
  Z[, k$terms, drop = FALSE] / s
}

# seconds needed to get from yardline_100 into field goal range (the opponent's
# 35) and kick
wpfd_fg_range_time <- function(yl) {
  10 + 2 * pmax(yl - 35, 0)
}

# smooth field goal make probability for the offset (the same curve as
# nflfastR's vegas_wp)
wpfd_fg_make <- function(yl) {
  stats::plogis(4.26 - 0.111 * yl)
}

# weight of the whole offset
wpfd_fade <- function(g, k) {
  ifelse(g >= k$fade_t, 1, k$fade_end + (1 - k$fade_end) * g / k$fade_t)
}

# offset on the logit scale
wpfd_offset <- function(x, beta, k) {
  Z <- wpfd_design(x, k)
  s <- colnames(Z) %in% wpfd_score_terms
  eta <- k$shrink *
    drop(Z[, s, drop = FALSE] %*% beta[s]) +
    drop(Z[, !s, drop = FALSE] %*% beta[!s])
  p <- stats::pnorm(eta)
  wpfd_fade(x$game_seconds_remaining, k) *
    stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}

wpfd_cache <- new.env(parent = emptyenv())

wpfd_booster <- function(model = wp_first_down_model) {
  if (is.null(wpfd_cache$booster)) {
    tryCatch(
      wpfd_cache$booster <- xgboost::xgb.load.raw(model$trees_raw),
      error = function(e) {
        cli::cli_abort(c(
          "Can't load the win probability model.",
          "i" = paste(
            "It was saved with xgboost {.val {model$xgboost_version}} and you",
            "have {.val {as.character(utils::packageVersion('xgboost'))}}."
          ),
          "x" = conditionMessage(e)
        ))
      }
    )
  }
  wpfd_cache$booster
}

# xgboost changed `iterationrange` from a half open [start, end) range (1.x) to
# an inclusive one (3.x). Find out which one the installed version uses by
# comparing the first n_trees rounds with the full model (they are the same
# model only if the range is inclusive).
wpfd_inclusive_range <- function(booster, model = wp_first_down_model) {
  if (is.null(wpfd_cache$inclusive)) {
    k <- model$constants
    x <- data.frame(
      game_seconds_remaining = c(3000, 1500, 400, 90),
      half_seconds_remaining = c(1200, 1500, 400, 90),
      score_differential = c(0, 3, -4, 7),
      posteam_spread = c(-3, 6.5, 2.5, -1),
      home = c(1, 0, 1, 0),
      receive_2h_ko = c(1, 0, 0, 0),
      yardline_100 = c(75, 42, 18, 3),
      posteam_timeouts_remaining = c(3, 2, 1, 0),
      defteam_timeouts_remaining = c(3, 3, 2, 1)
    )
    dm <- xgboost::xgb.DMatrix(wpfd_tree_matrix(x, k))
    full <- stats::predict(booster, dm, outputmargin = TRUE)
    upto <- stats::predict(
      booster,
      dm,
      outputmargin = TRUE,
      iterationrange = c(1L, k$n_trees)
    )
    wpfd_cache$inclusive <- isTRUE(all.equal(
      as.numeric(full),
      as.numeric(upto)
    ))
  }
  wpfd_cache$inclusive
}

# margin of the first n trees. The model is stored with base_score 0.5, so the
# margin has no intercept of its own.
wpfd_tree_margin <- function(booster, X, n, inclusive) {
  if (n <= 0) {
    return(rep(0, nrow(X)))
  }
  as.numeric(stats::predict(
    booster,
    xgboost::xgb.DMatrix(X),
    outputmargin = TRUE,
    iterationrange = c(1L, if (inclusive) n else n + 1L)
  ))
}

# logit of the WP of the team with the ball: offset plus trees
wpfd_logit <- function(x, model = wp_first_down_model) {
  k <- model$constants
  n <- nrow(x)
  if (n == 0) {
    return(numeric(0))
  }
  booster <- wpfd_booster(model)
  inclusive <- wpfd_inclusive_range(booster, model)
  X <- wpfd_tree_matrix(x, k)
  sch <- k$schedule[order(k$schedule$t), ]
  g <- pmin(pmax(x$game_seconds_remaining, min(sch$t)), max(sch$t))
  j <- pmin(findInterval(g, sch$t), nrow(sch) - 1L)
  j[is.na(j)] <- 1L
  w <- (g - sch$t[j]) / (sch$t[j + 1L] - sch$t[j])
  ns <- unique(sch$n)
  F <- lapply(ns, function(nt) wpfd_tree_margin(booster, X, nt, inclusive))
  names(F) <- as.character(ns)
  tm <- numeric(n)
  for (jj in seq_len(nrow(sch) - 1L)) {
    i <- which(j == jj)
    a <- F[[as.character(sch$n[jj])]][i]
    b <- F[[as.character(sch$n[jj + 1L])]][i]
    tm[i] <- (1 - w[i]) * a + w[i] * b
  }
  wpfd_offset(x, model$beta, k) + tm
}

# The end of the game, as in nflfastR's vegas_wp (nflfastR#625).
#
# In the last minutes the trees (up to 1,500, with few plays per state) put
# steps of several points between consecutive seconds and adjacent yard lines.
# The clock at the snap and the length of a play vary by more than a second,
# and the spot by more than a yard, so there the logit is averaged over 11
# points within +-wpfd_end_smooth seconds and +-wpfd_end_yards yards. Each
# point has its own clock and yard offset (each yard offset is used 2 or 3
# times, symmetric around the state and uncorrelated with the clock offset),
# which smooths about as well as all 55 combinations at the cost of 11. The
# clock window narrows to nothing at 0:00 and at wpfd_end_window seconds left,
# the yard window at either goal line.
wpfd_end_smooth <- 16
wpfd_end_yards <- 2
wpfd_end_window <- 240
wpfd_end_u <- seq(-1, 1, length.out = 11)
wpfd_end_v <- c(1, -2, 0, 2, -1, 0, 1, -2, 0, 2, -1) / 2

# WP of the team with the ball, without the touchdown cap and the field goal
# floor
wpfd_raw <- function(x, model = wp_first_down_model) {
  if (nrow(x) == 0) {
    return(numeric(0))
  }
  l <- wpfd_logit(x, model)
  t <- x$game_seconds_remaining
  i <- which(!is.na(l) & !is.na(t) & t > 0 & t < wpfd_end_window)
  if (length(i) > 0) {
    xi <- x[i, , drop = FALSE]
    r <- pmin(wpfd_end_smooth, t[i], wpfd_end_window - t[i])
    yl <- xi$yardline_100
    ry <- pmin(wpfd_end_yards, yl - 1, 99 - yl)
    ry[is.na(ry)] <- 0
    acc <- 0
    for (k in seq_along(wpfd_end_u)) {
      xi$game_seconds_remaining <- xi$half_seconds_remaining <- t[i] +
        wpfd_end_u[k] * r
      xi$yardline_100 <- yl + round(wpfd_end_v[k] * ry)
      acc <- acc + wpfd_logit(xi, model)
    }
    l[i] <- acc / length(wpfd_end_u)
  }
  p <- stats::plogis(l)
  # every state this model values is a 1st down
  down <- if (is.null(x[["down"]])) {
    1
  } else {
    dplyr::coalesce(as.numeric(x$down), 1)
  }
  # A team ahead with the ball wins if it can kneel out the clock, and nearly
  # always if it is a few seconds short
  kn <- which(!is.na(p) & x$score_differential > 0 & t >= 0 & t < 300)
  if (length(kn) > 0) {
    over <- t[kn] -
      wpfd_kneel_time(
        t[kn],
        rep_len(down, nrow(x))[kn],
        x$defteam_timeouts_remaining[kn]
      )
    w <- pmin(pmax((40 - over) / 30, 0), 1)
    p[kn] <- pmax(p[kn], (1 - w) * p[kn] + w * wpfd_kneel_value)
  }
  # A team behind deep in its own half with seconds left rarely wins: WP moves
  # to 0 over the last wpfd_behind_t seconds, fading in from the own 45 to the
  # own 35
  bh <- which(
    !is.na(p) &
      x$score_differential < 0 &
      x$yardline_100 > 55 &
      t >= 0 &
      t < wpfd_behind_t
  )
  if (length(bh) > 0) {
    wy <- pmin((x$yardline_100[bh] - 55) / 10, 1)
    p[bh] <- p[bh] * (1 - wy * (1 - t[bh] / wpfd_behind_t))
  }
  p
}
wpfd_behind_t <- 20

# Seconds a team can run off by kneeling: each kneel takes about 3 seconds and
# 40 more run off between kneels unless the other team stops the clock (its
# timeouts, and the 2 minute warning once). WP is wpfd_kneel_value up to 10
# seconds beyond this, moving to the model's value at 40.
wpfd_kneel_time <- function(t, down, dto) {
  n <- 5 - down
  stops <- dto + as.numeric(t > 120)
  3 * n + 40 * pmax(n - 1 - stops, 0)
}
wpfd_kneel_value <- 0.9998

# seconds left after a kick at the end of the game below which the field goal
# value moves toward the final result
wpfd_fg_end_t <- 15

# value of the final score of regulation for the team with the ball (a tie
# goes to overtime)
wpfd_end_value <- function(sd, tie = 0.5) {
  ifelse(sd > 0, 1, ifelse(sd < 0, 0, tie))
}

# WP of the team with the ball at the start of overtime, before the coin toss,
# from nflfastR's overtime model (0.5 with an nflfastR that doesn't have it)
wpfd_ot_start <- function(x) {
  f <- get0("wp_ot_start", envir = asNamespace("nflfastR"), inherits = FALSE)
  if (is.null(f)) {
    return(rep(0.5, nrow(x)))
  }
  n <- nrow(x)
  season <- if (is.null(x[["season"]])) rep(NA_real_, n) else x[["season"]]
  type <- if (is.null(x[["type"]])) rep("reg", n) else x[["type"]]
  f(data.frame(
    season = season,
    season_type = ifelse(type %in% "post", "POST", "REG"),
    posteam_spread = x$posteam_spread
  ))
}

# The state of the next possession after the offense scores `pts` on this snap:
# the other team has 1st and 10 at the typical start after a kickoff, a few
# seconds later
wpfd_post_td_state <- function(x, pts, dt = 6) {
  y <- x
  y$score_differential <- -(x$score_differential + pts)
  y$posteam_spread <- -x$posteam_spread
  y$home <- 1 - x$home
  y$ydstogo <- 10
  y$yardline_100 <- kickoff_yardline(x$season)
  y$posteam_timeouts_remaining <- x$defteam_timeouts_remaining
  y$defteam_timeouts_remaining <- x$posteam_timeouts_remaining
  first <- x$game_seconds_remaining > 1800
  y$receive_2h_ko <- ifelse(first, 1 - x$receive_2h_ko, 0)
  y$game_seconds_remaining <- pmax(x$game_seconds_remaining - dt, 0)
  y$half_seconds_remaining <- pmax(x$half_seconds_remaining - dt, 0)
  y
}

# probability that a field goal from the current spot is good, from the same
# table as field goal decisions (see get_fg_wp()). Without an fg_model_roof
# column it is built from roof (outdoors if there is no roof column) and season
wpfd_fg_prob <- function(x) {
  key <- x[["fg_model_roof"]]
  if (is.null(key)) {
    outdoors <- if (is.null(x[["roof"]])) TRUE else x[["roof"]] %in% "outdoors"
    key <- paste0(as.integer(outdoors), get_fg_era(x$season))
  }
  i <- match(
    paste(as.integer(x$yardline_100), as.character(key)),
    paste(fg_prob_df$yardline_100, as.character(fg_prob_df$fg_model_roof))
  )
  dplyr::coalesce(fg_prob_df$fg_make_prob[i], 0)
}

# probability that the PAT after a touchdown is good: a field goal from the 15
# since 2015 (the 2 before)
wpfd_pat_prob <- function(x) {
  x$yardline_100 <- ifelse(x$season >= 2015, 15, 2)
  wpfd_fg_prob(x)
}

# WP of kicking a field goal on this snap: made, the other team receives the
# kickoff; missed, it takes over at the spot of the kick, 8 yards behind the
# line of scrimmage (or its 20)
wpfd_fg_value <- function(x, model, p_fg, dt = 5) {
  made <- wpfd_post_td_state(x, 3, dt)
  miss <- made
  miss$score_differential <- -x$score_differential
  miss$yardline_100 <- pmin(100 - (x$yardline_100 + 8), 80)
  # a kick at the end of the 1st half ends the half: value it just before
  # halftime, where the 2nd half kickoff still counts
  first <- x$game_seconds_remaining > 1800
  h <- ifelse(
    first,
    pmax(x$half_seconds_remaining - dt, 1),
    made$half_seconds_remaining
  )
  g <- ifelse(first, 1800 + h, made$game_seconds_remaining)
  made$half_seconds_remaining <- miss$half_seconds_remaining <- h
  made$game_seconds_remaining <- miss$game_seconds_remaining <- g
  v_made <- 1 - wpfd_raw(made, model)
  v_miss <- 1 - wpfd_raw(miss, model)
  # A kick at the end of the game decides it: made, the team wins if it is
  # ahead; missed, the score stands (a tie goes to overtime). The model doesn't
  # know that the game ends, so with up to wpfd_fg_end_t seconds left after the
  # kick its value is blended toward the final result.
  w <- ifelse(
    first,
    1,
    pmin(pmax(x$game_seconds_remaining - dt, 0) / wpfd_fg_end_t, 1)
  )
  tie <- rep(0.5, nrow(x))
  end <- which(w < 1)
  if (length(end) > 0) {
    tie[end] <- wpfd_ot_start(x[end, , drop = FALSE])
  }
  v_made <- w * v_made + (1 - w) * wpfd_end_value(x$score_differential + 3, tie)
  v_miss <- w * v_miss + (1 - w) * wpfd_end_value(x$score_differential, tie)
  p_fg * v_made + (1 - p_fg) * v_miss
}

# WP of the team with the ball (regulation). WP is at least the value of
# kicking a field goal on this snap. It is capped at the value of scoring a
# touchdown on this snap, except in the last five minutes (where not scoring
# can be the better play) and in the last seconds of a half (where the next
# possession is not a kickoff return).
wpfd_predict <- function(x, model = wp_first_down_model, dt = 6) {
  x <- as.data.frame(x)
  w <- wpfd_raw(x, model)
  p_fg <- wpfd_fg_prob(x)
  fl <- which(p_fg > 0 & !is.na(w))
  if (length(fl) > 0) {
    xf <- x[fl, , drop = FALSE]
    w[fl] <- pmax(w[fl], wpfd_fg_value(xf, model, p_fg[fl]), na.rm = TRUE)
  }
  cap <- which(
    x$game_seconds_remaining > 300 &
      x$half_seconds_remaining > dt &
      !is.na(w)
  )
  if (length(cap) > 0) {
    xc <- x[cap, , drop = FALSE]
    pat <- wpfd_pat_prob(xc)
    post <- pat *
      (1 - wpfd_raw(wpfd_post_td_state(xc, 7, dt), model)) +
      (1 - pat) * (1 - wpfd_raw(wpfd_post_td_state(xc, 6, dt), model))
    w[cap] <- pmin(w[cap], post)
  }
  w
}

drop.me <- c("vegas_wp", "vegas_home_wp", "ep")

# Win probability of the states in `pbp_data`, from the point of view of the
# team that has the 4th down decision (`original_posteam`). Regulation states
# are the average of nfl4th's model above and nflfastR's vegas_wp, overtime
# states come from nflfastR's overtime model. Rows can carry `ot_end`, the final
# WP (again for `original_posteam`) of states where the overtime rules end the
# game.
calculate_win_probability <- function(pbp_data) {
  pbp_data <- pbp_data |> dplyr::select(-dplyr::any_of(drop.me))
  pbp_data <- pbp_data |> dplyr::select(-ends_with("_prob"))
  if (nrow(pbp_data) == 0) {
    pbp_data$vegas_wp <- numeric(0)
    return(pbp_data)
  }

  x <- wp_inputs(pbp_data)
  fastr <- fastr_wp(x)

  wp <- rep(NA_real_, nrow(x))
  ot <- x$qtr > 4
  reg <- which(!ot)
  if (length(reg) > 0) {
    wp[reg] <- (wpfd_predict(x[reg, , drop = FALSE]) + fastr[reg]) / 2
  }
  if (any(ot)) {
    wp[ot] <- ot_win_probability(x[ot, , drop = FALSE], fastr[ot])
  }

  # from the point of view of the team with the 4th down decision
  wp <- ifelse(x$posteam != x$original_posteam, 1 - wp, wp)
  if ("ot_end" %in% names(x)) {
    wp <- ifelse(!is.na(x$ot_end), x$ot_end, wp)
  }
  pbp_data$vegas_wp <- wp
  pbp_data
}

# columns the win probability models need, from the point of view of the team
# with the ball
wp_inputs <- function(pbp_data) {
  pbp_data |>
    dplyr::mutate(
      home = as.numeric(posteam == home_team),
      posteam_spread = dplyr::if_else(home == 1, spread_line, -spread_line),
      receive_2h_ko = dplyr::case_when(
        # 1st half, home team opened game with kickoff, away team has ball
        qtr <= 2 & home_opening_kickoff == 1 & posteam == away_team ~ 1,
        # 1st half, away team opened game with kickoff, home team has ball
        qtr <= 2 & home_opening_kickoff == 0 & posteam == home_team ~ 1,
        TRUE ~ 0
      ),
      posteam_timeouts_remaining = dplyr::if_else(
        home == 1,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        home == 1,
        away_timeouts_remaining,
        home_timeouts_remaining
      )
    )
}

# nflfastR's win probability for the team with the ball: vegas_wp in
# regulation, the overtime model in overtime. `x` comes from wp_inputs()
fastr_wp <- function(x) {
  nflfastR::calculate_win_probability(data.frame(
    receive_2h_ko = x$receive_2h_ko,
    home_team = x$home_team,
    posteam = x$posteam,
    score_differential = x$score_differential,
    half_seconds_remaining = x$half_seconds_remaining,
    game_seconds_remaining = x$game_seconds_remaining,
    spread_line = x$spread_line,
    down = x$down,
    ydstogo = x$ydstogo,
    yardline_100 = x$yardline_100,
    posteam_timeouts_remaining = x$posteam_timeouts_remaining,
    defteam_timeouts_remaining = x$defteam_timeouts_remaining,
    qtr = x$qtr,
    season = x$season,
    season_type = toupper(x$type),
    ot_possession = x$ot_possession,
    roof = x$roof,
    stringsAsFactors = FALSE
  ))$vegas_wp
}

# nflfastR's win probability of the states in `pbp_data` for the team with the
# ball, without the average with nfl4th's own model (see fd_late())
fastr_win_probability <- function(pbp_data) {
  if (nrow(pbp_data) == 0) {
    return(numeric(0))
  }
  fastr_wp(wp_inputs(pbp_data))
}
