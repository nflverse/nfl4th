library(tidyverse)
library(tidymodels)
# for getting data ready for the model
source(paste0(
  "https://raw.githubusercontent.com/nflverse/nflfastR/master/R/",
  "helper_add_nflscrapr_mutations.R"
))

# The go for it model that used to be built here (a 76 class xgboost
# classifier of yards gained, saved as data-raw/fd_model.Rdata and downloaded
# from the model_archive release) is replaced by data-raw/_go_for_it_model.R.

# ******************************************************************************
# 2pt model

if (grepl("Documents", getwd())) {
  thread <- 4
} else {
  ### server
  thread <- 6
}

seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue(
        "https://raw.githubusercontent.com/nflverse/nflfastR-data/master/",
        "data/play_by_play_{x}.rds"
      )
    )
  )
}) |>
  filter(
    is.na(down),
    !is.na(two_point_conv_result),
    yardline_100 == 2,
    rush == 1 | pass == 1,
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential),
    week <= 17
  ) |>
  make_model_mutations()

model_data <- pbp |>
  mutate(
    label = if_else(two_point_conv_result == "success", 1, 0),
    home_total = (spread_line + total_line) / 2,
    away_total = (total_line - spread_line) / 2,
    posteam_total = if_else(posteam == home_team, home_total, away_total),
    posteam_spread = dplyr::if_else(
      posteam == home_team,
      spread_line,
      -1 * spread_line
    )
  ) |>
  select(
    label,
    era2,
    era3,
    era4,
    outdoors,
    retractable,
    dome,
    posteam_spread,
    total_line,
    posteam_total
  )


full_train <- xgboost::xgb.DMatrix(
  model.matrix(~ . + 0, data = model_data |> select(-label)),
  label = model_data$label
)

#params
nrounds <- 15000

grid <- dials::grid_latin_hypercube(
  dials::finalize(dials::mtry(), model_data |> select(-label)),
  # dials::min_n(range = c(2, 3)),
  # dials::tree_depth(range = c(7, 9)),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  loss_reduction(range = c(-10, -1), trans = log10_trans()),
  sample_size = dials::sample_prop(),
  size = 40
) |>
  mutate(
    # has to be between 0 and 1
    mtry = mtry / length(model_data |> select(-label))
  )

grid |>
  head(20)

get_metrics <- function(df, row = 1) {
  # testing only
  # df <- grid |> dplyr::slice(1)

  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample = df$sample_size,
      colsample_bytree = df$mtry,
      max_depth = 8,
      min_child_weight = 2,
      monotone_constraints = "(0, 0, 0, 0, 0, 0, 1, 0, 1)",
      nthread = thread
    )
  #
  # era2, 0
  # era3, 0
  # era4,0
  # outdoors, 0
  # retractable, 0
  # dome, 0
  # posteam_spread, 1
  # total_line, 0
  # posteam_total 1

  #train
  wp_cv_model <- xgboost::xgb.cv(
    data = full_train,
    params = params,
    nrounds = nrounds,
    nfold = 5,
    metrics = list("logloss"),
    early_stopping_rounds = 20,
    print_every_n = 50
  )

  output <- params
  output$iter <- wp_cv_model$best_iteration
  output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  output$error <- wp_cv_model$evaluation_log[output$iter]$test_error_mean

  this_param <- bind_rows(output)

  if (row == 1) {
    saveRDS(this_param, "data/modeling.rds")
  } else {
    prev <- readRDS("data/modeling.rds")
    for_save <- bind_rows(prev, this_param)
    saveRDS(for_save, "data/modeling.rds")
  }

  return(this_param)
}

# do this piece by piece so server doesn't die
# actual code:
# 1 : nrow(grid)

# get results
results <- map_df(1:nrow(grid), function(x) {
  gc()
  message(glue::glue("Row {x}"))
  get_metrics(grid |> dplyr::slice(x), row = x)
})


# plot
results |>
  select(
    logloss,
    eta,
    gamma,
    subsample,
    colsample_bytree,
    max_depth,
    min_child_weight
  ) |>
  pivot_longer(
    eta:min_child_weight,
    values_to = "value",
    names_to = "parameter"
  ) |>
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

results |>
  arrange(logloss) |>
  select(
    eta,
    gamma,
    subsample,
    colsample_bytree,
    max_depth,
    min_child_weight,
    iter,
    logloss
  )


# final best model
#
# eta 0.0576
# gamma 0.0006930406
# subsample 0.395
# colsample_bytree 0.444
# max_depth 8
# min_child_weight 2
# iter 20
# logloss 0.6825118

params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.0576,
    gamma = 0.0006930406,
    subsample = 0.395,
    colsample_bytree = 0.444,
    max_depth = 8,
    min_child_weight = 2,
    monotone_constraints = "(0, 0, 0, 0, 0, 0, 1, 0, 1)",
    nthread = thread
  )


#train
wp_cv_model <- xgboost::xgb.cv(
  data = full_train,
  params = params,
  nrounds = nrounds,
  nfold = 5,
  metrics = list("logloss"),
  early_stopping_rounds = 20,
  print_every_n = 50
)


# ******************************************************************************
# train

nrounds <- 21
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.0576,
    gamma = 0.0006930406,
    subsample = 0.395,
    colsample_bytree = 0.444,
    max_depth = 8,
    min_child_weight = 2,
    monotone_constraints = "(0, 0, 0, 0, 0, 0, 1, 0, 1)",
    nthread = thread
  )

two_pt_model <- xgboost::xgboost(
  params = params,
  data = full_train,
  nrounds = nrounds,
  verbose = 2
)

save(two_pt_model, file = 'data-raw/two_pt_model.Rdata')

importance <- xgboost::xgb.importance(
  feature_names = colnames(two_pt_model),
  model = two_pt_model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)
