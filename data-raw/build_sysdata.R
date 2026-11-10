# non-xgboost-models
load("data-raw/fg_model.Rdata")
punt_df <- readRDS("data-raw/punt_data.rds")
# selection-corrected field goal make probability by field position and
# roof/era group, built in data-raw/_punt_and_fg_models.R
fg_prob_df <- readRDS("data-raw/fg_prob.rds")

# xgboost-model from model archive
piggyback::pb_download(
  file = "two_pt_model.ubj",
  tag = "model_archive",
  dest = "data-raw"
)
two_pt_model <- xgboost::xgb.load("data-raw/two_pt_model.ubj") |>
  xgboost::xgb.save.raw("ubj")

# fg_model is kept even though make probability now comes from fg_prob_df:
# get_2pt_wp() still predicts the PAT from it, at a distance where the raw
# curve is fine (no selection bias on extra points).
# two point models and fg_model live in the package together with the punt_df
# win probability model for 1st down states, built in data-raw/_wp_model.R
wp_first_down_model <- readRDS("data-raw/wp_first_down_model.rds")

# The go for it model (fd_model) is too big to keep in the package. It is
# loaded and cached on first use.
usethis::use_data(
  two_pt_model,
  fg_model,
  fg_prob_df,
  punt_df,
  wp_first_down_model,
  no_cache,
  internal = TRUE,
  overwrite = TRUE
)
