nfl4th_cache_dir <- rappdirs::user_cache_dir("nfl4th", "nflverse")
nfl4th_games_path <- file.path(nfl4th_cache_dir, "games_nfl4th.rds")
nfl4th_model_path <- file.path(nfl4th_cache_dir, "fd_model.rds")

.onLoad <- function(libname,pkgname){
  is_online <- !is.null(curl::nslookup("github.com", error = FALSE))
  # create package cache directory if it doesn't exist
  if (!dir.exists(nfl4th_cache_dir)){
    dir.create(nfl4th_cache_dir, recursive = TRUE, showWarnings = FALSE)
  } else if (file.exists(nfl4th_games_path) && !getOption("nfl4th.keep_games", !is_online)){
    # remove games from package cache on load so it updates once
    # does only run if options(nfl4th.keep_games) != TRUE and runner has access
    # to github.com. If option is not set, the file will be removed is online.
    file.remove(nfl4th_games_path)
  }
#.onLoad <- function(libname, pkgname) {
#  .games_nfl4th <- get_games_file()
#  fd_model <- load_fd_model()
#  wp_model <- load_wp_model()
#  assign(".games_nfl4th", .games_nfl4th, envir = parent.env(environment()))
#  assign("fd_model", fd_model, envir = parent.env(environment()))
#  assign("wp_model", wp_model, envir = parent.env(environment()))
#}
