# these helpers read the games file and save it to a package cache

nfl4th_games_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "games_nfl4th.rds")
}
# the go for it and win probability models used to be downloaded and cached
# here. They now live in the package, the paths are kept so that
# nfl4th_clear_cache() can remove old files
nfl4th_fdmodel_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "fd_model.rds")
}
nfl4th_wpmodel_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "wp_model.rds")
}

# the games file in memory, read again only when the file changes
nfl4th_games_memo <- new.env(parent = emptyenv())

.games_nfl4th <- function() {
  if (probably_cran() && !force_cache()) {
    return(get_games_file())
  }
  f <- nfl4th_games_path()
  if (!file.exists(f)) {
    saveRDS(get_games_file(), f)
  }
  info <- file.info(f)[c("mtime", "size")]
  if (
    is.null(nfl4th_games_memo$data) || !identical(nfl4th_games_memo$info, info)
  ) {
    nfl4th_games_memo$data <- readRDS(f)
    nfl4th_games_memo$info <- info
  }
  nfl4th_games_memo$data
}

#' Reset nfl4th Package Cache
#'
#' @param type One of `"games"` (the default), `"fd_model"`, `"wp_model"`, or
#'   `"all"`.
#'   `"games"` will remove an internally used games file.
#'   `"fd_model"` and `"wp_model"` will remove the go for it and win
#'   probability models cached by earlier nfl4th versions (both models now ship
#'   with the package).
#'   `"all"` will remove all of the above.
#'
#' @return Returns `TRUE` invisibly if cache has been cleared.
#' @export
#'
#' @examples
#' nfl4th_clear_cache()
nfl4th_clear_cache <- function(
  type = c("games", "fd_model", "wp_model", "all")
) {
  type <- rlang::arg_match(type)
  to_delete <- switch(
    type,
    "games" = nfl4th_games_path(),
    "fd_model" = nfl4th_fdmodel_path(),
    "wp_model" = nfl4th_wpmodel_path(),
    "all" = c(nfl4th_games_path(), nfl4th_fdmodel_path(), nfl4th_wpmodel_path())
  )
  file.remove(to_delete[file.exists(to_delete)])
  invisible(TRUE)
}

probably_cran <- function() {
  envvars <- vapply(
    X = c(
      "_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_",
      "_R_CHECK_THINGS_IN_OTHER_DIRS_",
      "_R_CHECK_THINGS_IN_OTHER_DIRS_XTRA_"
    ),
    FUN = \(x) Sys.getenv(x, unset = NA_character_),
    FUN.VALUE = character(1L),
    USE.NAMES = TRUE
  )
  cache_path <- path.expand(tools::R_user_dir("nfl4th", "cache"))
  any(!is.na(envvars), grepl(rawToChar(no_cache), x = cache_path))
}

# allow user to force the cache even if probably_cran() is TRUE
force_cache <- function() {
  getOption("nfl4th.force_cache", "false") == "true"
}
