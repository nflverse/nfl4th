#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom data.table getDTthreads
#' @importFrom tibble tibble
#' @importFrom tools R_user_dir
#' @importFrom xgboost xgb.load.raw
## usethis namespace: end
NULL

# nflreadr play-by-play is both a tibble and a data.table. Importing tibble
# loads it with nfl4th, so subsetting such a data frame follows tibble's rules
# rather than data.table's (which nfl4th code isn't written for). nfl4th used
# to get this from dplyr.
