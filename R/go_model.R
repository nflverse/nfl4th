# Go for it model: distribution of yards gained when going for it on 4th down.
#
# Built by data-raw/_go_for_it_model.R and stored in R/sysdata.rda as
# `fd_tables`: smooth models of
#   1. conversion (gaining the line to gain, or a first down by penalty),
#   2. a touchdown given a conversion that isn't goal to go,
#   3. where a conversion that doesn't score ends,
#   4. where a failed attempt ends,
# tabulated for 4th down by distance and field position. Spread and total
# shift the conversion and touchdown logits (the spread's effect on conversion
# depends on distance), and the era (see get_fd_era()) and a late or desperate
# situation (see fd_is_late()) shift conversion by amounts that depend on
# distance.

# Late and desperate plays: the last 10 minutes of the 4th quarter (or
# overtime) while trailing, or a win probability below fd_late_wp for the team
# with the ball (nfl4th's WP model, regulation only). Late 4th and long
# attempts convert more often than normal ones (2001-2026, not goal to go, 4th
# and 8-10: 36.1% vs 27.8%; 11+: 23.0% vs 20.8%), against defenses that give
# up more underneath. The last 2 minutes of the 1st half aren't late: those
# 4th downs convert like normal ones or worse (4-5 yards: 38.6% vs 46.5%
# normal and 49.6% late; 8-10: 24.3% vs 27.8% and 36.1%). The go for it model
# is fit with this flag and data-raw/_go_for_it_model.R uses this function
# too. Missing values count as not late.
fd_is_late <- function(qtr, score_differential, game_seconds_remaining, wp) {
  coalesce(
    (qtr >= 4 & score_differential < 0 & game_seconds_remaining <= 600) |
      (!is.na(wp) & wp < fd_late_wp),
    FALSE
  )
}

# The WP below which a play is late. nfl4th's WP model has no down or
# distance, so it values a 4th down state like a 1st down at the same spot,
# about 4.5 points above nflfastR's WP of the 4th down state, which the flag
# used before. In season grouped CV of the go for it model's conversion part
# (2001-2026), thresholds from 0.15 to 0.19 do equally well on 4th downs (log
# loss within 0.0001), 0.15 the best, and all slightly better than the old flag
# (nflfastR's WP below 0.15: -0.0002, se 0.00015).
fd_late_wp <- 0.15

# fd_is_late() for prepared 4th down plays, with nfl4th's WP model's value of
# the play's state (fd_state_wp(), as in data-raw/_go_for_it_model.R). In a
# bootstrap draw it is that draw's WP model, as in the draw's fit.
fd_late <- function(pbp) {
  # a play trailing in the last 10 minutes is late whatever its win
  # probability, so the WP is only computed for the others
  trailing_late <- pbp$qtr >= 4 &
    pbp$score_differential < 0 &
    pbp$game_seconds_remaining <= 600
  wp <- rep(NA_real_, nrow(pbp))
  need <- which(!(trailing_late %in% TRUE) & !((pbp$qtr > 4) %in% TRUE))
  if (length(need) > 0) {
    wp[need] <- wpfd_predict(wp_inputs(pbp[need, , drop = FALSE]))
  }
  fd_is_late(pbp$qtr, pbp$score_differential, pbp$game_seconds_remaining, wp)
}

# Probability that a failed 4th down attempt is returned for a touchdown, by
# distance, field position and era (def_td_prob in R/sysdata.rda, from
# data-raw/_defensive_td_model.R). The same in every bootstrap draw, like the
# punt and field goal tables.
fd_def_td_prob <- function(x) {
  d <- dim(def_td_prob)
  t <- pmin(pmax(as.integer(x$ydstogo), 1L), d[1])
  yl <- pmin(pmax(as.integer(x$yardline_100), 1L), d[2])
  def_td_prob[cbind(t, yl, get_fd_era(x$season) + 1L)]
}

# Long data frame of gain probabilities for the plays in `x` (needs ydstogo,
# yardline_100, posteam_spread, total_line and season). A gain equal to
# yardline_100 is a touchdown. Failed attempts never end behind the offense's
# own 1. `late` marks late and desperate plays (see fd_is_late()), recycled to
# the rows of x. Returns columns row (index into x), gain and prob; the
# probabilities of each row sum to 1.
fd_gain_probs <- function(x, late = FALSE, tb = active_fd_tables()) {
  # plays stacked once per bootstrap draw (add_boot_probs()): each draw's
  # rows with its own tables, in the order of the rows
  per_draw <- nfl4th_active$fd_draws
  if (!is.null(per_draw) && !is.null(x[[".draw"]]) && nrow(x) > 0) {
    late <- rep_len(late, nrow(x))
    parts <- lapply(split(seq_len(nrow(x)), x$.draw), function(i) {
      g <- fd_gain_probs_tb(
        x[i, , drop = FALSE],
        late[i],
        per_draw[[x$.draw[i[1]]]]
      )
      g$row <- i[g$row]
      g
    })
    out <- do.call(rbind, unname(parts))
    out <- out[order(out$row, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  fd_gain_probs_tb(x, late, tb)
}

fd_gain_probs_tb <- function(x, late, tb) {
  n <- nrow(x)
  if (n == 0) {
    return(data.frame(row = integer(), gain = integer(), prob = numeric()))
  }
  t_max <- nrow(tb$conv)
  r_max <- dim(tb$hazard)[2]
  e_max <- dim(tb$hazard)[3]
  y <- pmin(pmax(as.integer(x$yardline_100), 1L), 99L)
  t_raw <- pmax(as.integer(x$ydstogo), 1L)
  t_raw <- pmin(t_raw, y)
  t <- pmin(t_raw, t_max)
  era <- match(as.character(get_fd_era(x$season)), colnames(tb$conv_era))
  spread <- coalesce(as.numeric(x$posteam_spread), 0)
  ztot <- coalesce(as.numeric(x$total_line), tb$ref_total) - tb$ref_total
  late <- rep_len(coalesce(as.numeric(late), 0), n)
  # conversion and touchdown: logits at spread 0 and total 45 in the current
  # era in a normal situation, shifted by the game's spread, total, era and
  # situation. Distances beyond the table use its longest distance
  pc <- stats::plogis(
    tb$conv[cbind(t, y)] +
      tb$conv_era[cbind(t, era)] +
      tb$conv_late[t] * late +
      tb$conv_spread[t] * spread +
      tb$conv_coef[["total_line"]] * ztot
  )
  r <- y - t_raw
  ptd <- stats::plogis(
    tb$td[cbind(t, pmax(r, 1L))] +
      tb$td_coef[1] * spread +
      tb$td_coef[2] * ztot
  )
  out <- lapply(seq_len(n), function(i) {
    ti <- t_raw[i]
    yi <- y[i]
    if (r[i] <= 0) {
      # goal to go: a conversion is a touchdown
      succ_gain <- yi
      succ_prob <- pc[i]
    } else {
      # yards past the line to gain: probability of stopping at each yard
      # given the play got there, constant past the end of the table
      h <- tb$hazard[t[i], min(r[i], r_max), ]
      h <- c(h, rep(h[e_max], max(r[i] - e_max, 0L)))[seq_len(r[i])]
      ex <- h * cumprod(c(1, 1 - h))[seq_len(r[i])]
      ex <- ex / sum(ex)
      succ_gain <- c(ti + 0:(r[i] - 1L), yi)
      succ_prob <- pc[i] * c((1 - ptd[i]) * ex, ptd[i])
    }
    f <- tb$fail[[min(ti, length(tb$fail))]]
    k <- as.integer(names(f))
    k <- pmin(k, ti - 1L)
    # a failed attempt can't end behind the own 1
    k <- pmax(k, yi - 99L)
    fail_prob <- (1 - pc[i]) * as.numeric(f) / sum(f)
    fail_prob <- tapply(fail_prob, k, sum)
    list(
      c(succ_gain, as.integer(names(fail_prob))),
      c(succ_prob, as.numeric(fail_prob))
    )
  })
  # one data frame at the end (binding one per play was slow)
  gain <- lapply(out, `[[`, 1L)
  data.frame(
    row = rep(seq_len(n), lengths(gain)),
    gain = unlist(gain),
    prob = unlist(lapply(out, `[[`, 2L))
  )
}
