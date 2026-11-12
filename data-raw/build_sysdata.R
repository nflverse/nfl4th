# non-xgboost-models
load("data-raw/fg_model.Rdata")
# punt outcome probabilities by line of scrimmage, built in
# data-raw/_punt_model.R
punt_df <- readRDS("data-raw/punt_data.rds")
# selection-corrected field goal make probability by field position and
# roof/era group, built in data-raw/_punt_and_fg_models.R
fg_prob_df <- readRDS("data-raw/fg_prob.rds")

# fg_model is kept even though make probability now comes from fg_prob_df:
# get_2pt_wp() still predicts the PAT from it, at a distance where the raw
# curve is fine (no selection bias on extra points). 2 point tries use the go
# for it model (fd_tables), so there is no separate 2 point model.

# win probability model for 1st down states, built in data-raw/_wp_model.R
wp_first_down_model <- readRDS("data-raw/wp_first_down_model.rds")

# go for it model (yards gained when going for it), built in
# data-raw/_go_for_it_model.R
fd_tables <- readRDS("data-raw/fd_tables.rds")

# nflfastR's vegas_wp and overtime models and its field goal table, for the
# code copied from nflfastR in R/nflfastr_models.R (nflverse/nflfastR#625 at
# dc295839; nfl4th-checks/vendor/01_extract.R writes the .rds)
nflfastr_models <- readRDS("data-raw/nflfastr_models.rds")
wp_vegas_model <- nflfastr_models$wp_vegas_model
ot_wp_dp <- nflfastr_models$ot_wp_dp
ot_wp_models <- nflfastr_models$ot_wp_models
fg_prob_table <- nflfastr_models$fg_prob_table

usethis::use_data(
  fg_model,
  fg_prob_df,
  punt_df,
  wp_first_down_model,
  fd_tables,
  wp_vegas_model,
  ot_wp_dp,
  ot_wp_models,
  fg_prob_table,
  no_cache,
  internal = TRUE,
  overwrite = TRUE
)
