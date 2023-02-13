.onLoad <- function(libname,pkgname){
  
  is_online <- !is.null(curl::nslookup("github.com", error = FALSE))
  keep_games <- isTRUE(getOption("nfl4th.keep_games", FALSE))
  
  if(!is_online && !keep_games) rlang::warn("GitHub.com seems offline, and `options(nfl4th.keep_games)` is not set to TRUE. Deleting the games cache, and predictions may not be available without an internet connection.") 
  
  if(!is_online && keep_games) rlang::warn("GitHub.com seems offline, and `options(nfl4th.keep_games)` is set to TRUE. To get updates, clear the games cache with `nfl4th::nfl4th_clear_cache()`")

  # create package cache directory if it doesn't exist
  if (!dir.exists(rappdirs::user_cache_dir("nfl4th", "nflverse"))){
    dir.create(rappdirs::user_cache_dir("nfl4th", "nflverse"), recursive = TRUE, showWarnings = FALSE)
  } else if (file.exists(nfl4th_games_path()) && !keep_games){
    # remove games from package cache on load so it updates
    # only runs if options(nfl4th.keep_games) != TRUE
    # If option is not set, the file will be removed.
    file.remove(nfl4th_games_path())
  }
}
