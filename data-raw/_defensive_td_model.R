# Builds def_td_prob (R/sysdata.rda): the probability that a failed 4th down
# attempt is returned for a touchdown (an interception or a fumble), by
# distance (1 to fd_t_max), the line of scrimmage (yardline_100 1 to 99) and
# the go for it model's era (2001-2019 and 2020 on, get_fd_era()): an array
# [distance, yardline_100, era + 1]. get_go_wp() values that share of the failed attempts
# as a defensive touchdown (defensive_td_wp()). The go for it model's yards
# distribution leaves it out: a failed attempt ends at most 10 yards behind the
# line, so a pick six counts as the defense taking over there.
#
# Fit on the failed 3rd and 4th down attempts of the go for it model's training
# data (fd_model_data(), 2001 on): a logistic GAM in the line of scrimmage and
# the log of the distance, with a 4th down term and an era term. 4th downs alone
# have too few defensive touchdowns (57 of 7,040 failed attempts since 2001).
# - Field position: the closer the offense is to its own goal line, the
#   shorter the defense's return, and failures from its own 20 are returned
#   for a touchdown 2 to 3 times as often as failures inside the opponent's 20.
# - 4th downs: failures are returned for a touchdown about twice as often as
#   on 3rd down.
# - Era: the rate has fallen (about 0.66% of failed attempts in 2001-2015,
#   0.49% in 2016-2020 and 0.44% since). The era term lowers the AIC by 11 and
#   the season grouped CV log loss of all failed attempts (on 4th downs alone
#   the two are equal within noise).
# Defensive safeties (115 of 115,737 failed attempts, mostly sacks on 3rd
# down) are left out.
#
# Run from the package root.

library(dplyr)
library(mgcv)

FD_FUNCTIONS_ONLY <- TRUE
source("data-raw/_go_for_it_model.R")

extra <- c("touchdown", "td_team", "defteam")
pbp <- purrr::map(
  2001:nflreadr::most_recent_season(),
  \(s) {
    p <- nflreadr::load_pbp(s)
    p[p$down %in% c(3, 4), c(fd_cols, extra)]
  }
) |>
  bind_rows()

fail <- fd_model_data(pbp) |>
  filter(conv == 0) |>
  mutate(def_td = as.numeric(touchdown %in% 1 & td_team == defteam))
cat(
  "failed attempts:",
  nrow(fail),
  " defensive touchdowns:",
  sum(fail$def_td),
  " (4th downs:",
  sum(fail$def_td[fail$down == 4]),
  "of",
  sum(fail$down == 4),
  ")\n"
)

m <- gam(
  def_td ~ s(yl, k = 6) + s(lt, k = 4) + f4 + fera,
  family = binomial,
  data = fail,
  method = "REML"
)
print(summary(m))

# 4th downs: observed and fitted by field position
fail4 <- fail[fail$down == 4, ]
fail4$p <- predict(m, fail4, type = "response")
fail4 |>
  group_by(zone = cut(yl, c(0, 20, 40, 60, 80, 100))) |>
  summarise(
    n = n(),
    observed = mean(def_td),
    fitted = mean(p),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

g <- expand.grid(ydstogo = seq_len(fd_t_max), yl = 1:99, era = 0:1)
g$lt <- log(g$ydstogo)
g$f4 <- factor(1, levels = c(0, 1))
g$fera <- factor(g$era, levels = c(0, 1))
def_td_prob <- array(
  signif(as.numeric(predict(m, g, type = "response")), 6),
  dim = c(fd_t_max, 99, 2)
)
saveRDS(def_td_prob, "data-raw/def_td_prob.rds")

# R/sysdata.rda holds other objects: load them and write them back untouched
# so that only def_td_prob changes (see data-raw/_wp_model.R)
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), "def_td_prob")
before <- lapply(mget(untouched, envir = e), serialize, connection = NULL)
e$def_td_prob <- def_td_prob
save(
  list = ls(e),
  file = "R/sysdata.rda",
  envir = e,
  compress = "xz",
  version = 2
)
e2 <- new.env()
load("R/sysdata.rda", envir = e2)
stopifnot(identical(
  before,
  lapply(mget(untouched, envir = e2), serialize, connection = NULL)
))
