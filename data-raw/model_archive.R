## DO NOT USE THIS UNLESS YOU KNOW WHAT YOU ARE DOING ##

# this is the code that was used to re-save models in the model_archive tag
# of the nfl4th repo in order to fix xgboost compatibility issues with R serialized files

# We keep this only for future reference but we don't really need it anymore (hopefully)

load_fd_model <- function() {
  fd_model <- NULL
  con <- url(
    "https://github.com/guga31bb/fourth_calculator/blob/main/data/fd_model_v2.Rdata?raw=true"
  )
  try(load(con), silent = TRUE)
  close(con)
  fd_model
}

load_wp_model <- function() {
  wp_model <- NULL
  con <- url(
    "https://github.com/guga31bb/fourth_calculator/blob/main/data/home_wp_model.Rdata?raw=true"
  )
  try(load(con), silent = TRUE)
  close(con)
  wp_model
}

## xgboost models requires xgboost v ? to complete
load("data-raw/two_pt_model.Rdata")
two_pt_model <- xgboost::xgb.Booster.complete(two_pt_model)
fd_model <- load_fd_model() |> xgboost::xgb.Booster.complete()
wp_model <- load_wp_model() |> xgboost::xgb.Booster.complete()

xgboost::xgb.save(two_pt_model, "data-raw/two_pt_model.ubj")
xgboost::xgb.save(fd_model, "data-raw/fd_model.ubj")
xgboost::xgb.save(wp_model, "data-raw/wp_model.ubj")

## Read ubj files
two_pt_model <- xgboost::xgb.load("data-raw/two_pt_model.ubj") |>
  xgboost::xgb.Booster.complete() |>
  xgboost::xgb.save.raw("ubj")
fd_model <- xgboost::xgb.load("data-raw/fd_model.ubj") |>
  xgboost::xgb.Booster.complete() |>
  xgboost::xgb.save.raw("ubj")
wp_model <- xgboost::xgb.load("data-raw/wp_model.ubj") |>
  xgboost::xgb.Booster.complete() |>
  xgboost::xgb.save.raw("ubj")

saveRDS(two_pt_model, "data-raw/two_pt_model.rds")
saveRDS(fd_model, "data-raw/fd_model.rds")
saveRDS(wp_model, "data-raw/wp_model.rds")

## Read raw files
two_pt_model <- readRDS("data-raw/two_pt_model.rds") |> xgboost::xgb.load.raw()

## BAM
load("data-raw/fg_model.Rdata")
