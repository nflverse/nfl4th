# Go for it model: distribution of yards gained when going for it on 4th down.
#
# Built by data-raw/_go_for_it_model.R and stored in R/sysdata.rda as
# `fd_tables`: smooth models of
#   1. conversion (gaining the line to gain, or a first down by penalty),
#   2. a touchdown given a conversion that isn't goal to go,
#   3. where a conversion that doesn't score ends,
#   4. where a failed attempt ends,
# tabulated for 4th down by distance and field position. Spread and total
# shift the conversion and touchdown logits, and the era (see get_fd_era())
# shifts conversion.

# Long data frame of gain probabilities for the plays in `x` (needs ydstogo,
# yardline_100, posteam_spread, total_line and season). A gain equal to
# yardline_100 is a touchdown. Failed attempts never end behind the offense's
# own 1. Returns columns row (index into x), gain and prob; the probabilities of
# each row sum to 1.
fd_gain_probs <- function(x, tb = fd_tables) {
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
  era <- as.character(get_fd_era(x$season))
  spread <- dplyr::coalesce(as.numeric(x$posteam_spread), 0)
  ztot <- dplyr::coalesce(as.numeric(x$total_line), tb$ref_total) - tb$ref_total
  # conversion and touchdown: logits at spread 0 and total 45 in the current
  # era, shifted by the game's spread, total and era. Distances beyond the table
  # use its longest distance
  pc <- stats::plogis(
    tb$conv[cbind(t, y)] +
      tb$conv_era[era] +
      tb$conv_coef[1] * spread +
      tb$conv_coef[2] * ztot
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
    data.frame(
      row = i,
      gain = c(succ_gain, as.integer(names(fail_prob))),
      prob = c(succ_prob, as.numeric(fail_prob))
    )
  })
  do.call(rbind, out)
}
