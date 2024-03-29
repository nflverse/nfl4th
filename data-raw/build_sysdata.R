
# this is too big for CRAN so we put in a different repo
  # load("data-raw/fd_model.Rdata")

# basic non-xgb-models
  load("data-raw/fg_model.Rdata")
  punt_df <- readRDS("data-raw/punt_data.rds")

# load 2 pt model: will need to re-save
  load("data-raw/two_pt_model.Rdata")

# xgboost >= 1.6.0 warned the user because of old serialization formats.
# So we save the models in the suggested serialized json format, read them
# back in and save the in the package again.
  xgboost::xgb.save(two_pt_model, "two_pt_model.ubj")
  two_pt_model <- xgboost::xgb.load("two_pt_model.ubj") |> xgboost::xgb.Booster.complete()

usethis::use_data(two_pt_model, fg_model, punt_df, internal = TRUE, overwrite = TRUE)
