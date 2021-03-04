
load("data-raw/fd_model.Rdata")
load("data-raw/fg_model.Rdata")
load("data-raw/two_pt_model.Rdata")

punt_df <- readRDS("data-raw/punt_data.rds")

usethis::use_data(fd_model, two_pt_model, fg_model, punt_df, internal = TRUE, overwrite = TRUE)
