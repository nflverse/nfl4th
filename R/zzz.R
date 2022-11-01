nfl4th_cache_dir <- rappdirs::user_cache_dir("nfl4th", "nflverse")
nfl4th_games_path <- file.path(nfl4th_cache_dir, "games_nfl4th.rds")
nfl4th_fdmodel_path <- file.path(nfl4th_cache_dir, "fd_model.rds")
nfl4th_wpmodel_path <- file.path(nfl4th_cache_dir, "wp_model.rds")

.onLoad <- function(libname,pkgname){
  is_online <- !is.null(curl::nslookup("github.com", error = FALSE))
  # create package cache directory if it doesn't exist
  if (!dir.exists(nfl4th_cache_dir)){
    dir.create(nfl4th_cache_dir, recursive = TRUE, showWarnings = FALSE)
  } else if (file.exists(nfl4th_games_path) && !getOption("nfl4th.keep_games", !is_online)){
    # remove games from package cache on load so it updates once
    # does only run if options(nfl4th.keep_games) != TRUE and runner has access
    # to github.com. If option is not set, the file will be removed if online.
    file.remove(nfl4th_games_path)
  }
}
