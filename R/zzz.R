nfl4th_cache_dir <- rappdirs::user_cache_dir("nfl4th", "nflverse")
nfl4th_games_path <- file.path(nfl4th_cache_dir, "games_nfl4th.rds")
nfl4th_model_path <- file.path(nfl4th_cache_dir, "fd_model.rds")

.onLoad <- function(libname,pkgname){
  # create package cache directory if it doesn't exist
  if (!dir.exists(nfl4th_cache_dir)){
    dir.create(nfl4th_cache_dir, recursive = TRUE, showWarnings = FALSE)
  } else if (file.exists(nfl4th_games_path)){
    # remove games from package cache on load so it updates once
    file.remove(nfl4th_games_path)
  }
}
