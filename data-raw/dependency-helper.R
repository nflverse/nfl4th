# Run this to update the DESCRIPTION
imports <- c(
  "magrittr",
  "dplyr",
  "glue",
  "tidyr",
  "tibble",
  "nflfastR",
  "mgcv",
  "stringr",
  "tidyselect",
  "xgboost"
)
purrr::walk(imports, usethis::use_package, "Imports")
usethis::use_tidy_description()
rm(imports)
