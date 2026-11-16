# non-xgboost-models
load("data-raw/fg_model.Rdata")
# punt outcome probabilities by line of scrimmage, built in
# data-raw/_punt_model.R
punt_df <- readRDS("data-raw/punt_data.rds")
# selection-corrected field goal make probability by field position and
# roof/era group, built in data-raw/_punt_and_fg_models.R
fg_prob_df <- readRDS("data-raw/fg_prob.rds")

# The PAT make probability comes from fg_model, the raw field goal curve (a
# PAT has no selection bias), at the 15, by roof/era group. It is tabulated
# here, so the package needs neither mgcv nor fg_model. 2 point tries use the go
# for it model (fd_tables), so there is no separate 2 point model.
pat_levels <- levels(fg_model$model$fg_model_roof)
pat_make_prob <- stats::setNames(
  as.numeric(mgcv::predict.bam(
    fg_model,
    data.frame(
      yardline_100 = 15,
      fg_model_roof = factor(pat_levels, levels = pat_levels)
    ),
    type = "response"
  )),
  pat_levels
)

# win probability model for 1st down states, built in data-raw/_wp_model.R
wp_first_down_model <- readRDS("data-raw/wp_first_down_model.rds")

# go for it model (yards gained when going for it), built in
# data-raw/_go_for_it_model.R
fd_tables <- readRDS("data-raw/fd_tables.rds")

# nflfastR's overtime model, the kickoff spots of its vegas_wp model and its
# field goal table, for the code copied from nflfastR in R/nflfastr_models.R
# (nflverse/nflfastR#625 at 856232ab; data-raw/nflfastr_extract.R writes the
# .rds)
nflfastr_models <- readRDS("data-raw/nflfastr_models.rds")
wp_kick_start <- nflfastr_models$wp_kick_start
ot_wp_dp <- nflfastr_models$ot_wp_dp
ot_wp_models <- nflfastr_models$ot_wp_models
fg_prob_table <- nflfastr_models$fg_prob_table

# the clock after a 4th down conversion (conversion_runoff(),
# conversion_clock_out() and conversion_play_time()), written by
# data-raw/_conversion_runoff.R
conversion_clock <- readRDS("data-raw/conversion_clock.rds")

# the probability that a failed 4th down attempt is returned for a touchdown,
# written by data-raw/_defensive_td_model.R
def_td_prob <- readRDS("data-raw/def_td_prob.rds")

usethis::use_data(
  pat_make_prob,
  conversion_clock,
  def_td_prob,
  fg_prob_df,
  punt_df,
  wp_first_down_model,
  fd_tables,
  wp_kick_start,
  ot_wp_dp,
  ot_wp_models,
  fg_prob_table,
  no_cache,
  internal = TRUE,
  overwrite = TRUE
)
