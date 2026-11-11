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

usethis::use_data(
  fg_model,
  fg_prob_df,
  punt_df,
  wp_first_down_model,
  fd_tables,
  no_cache,
  internal = TRUE,
  overwrite = TRUE
)
