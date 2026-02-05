# non-xgboost-models
load("data-raw/fg_model.Rdata")
punt_df <- readRDS("data-raw/punt_data.rds")

# xgboost-model from model archive
piggyback::pb_download(
  file = "two_pt_model.ubj",
  tag = "model_archive",
  dest = "data-raw"
)
two_pt_model <- xgboost::xgb.load("data-raw/two_pt_model.ubj") |>
  xgboost::xgb.save.raw("ubj")

# two point models and fg_model live in the package together with the punt_df
# There are two other models (fd_model and wp_model) that are too big to keep
# them in the package. They are loaded and cached on package load.
usethis::use_data(
  two_pt_model,
  fg_model,
  punt_df,
  internal = TRUE,
  overwrite = TRUE
)
