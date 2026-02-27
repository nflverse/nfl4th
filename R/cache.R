# these helpers read games or models and save them to a package cache

nfl4th_games_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "games_nfl4th.rds")
}
nfl4th_fdmodel_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "fd_model.rds")
}
nfl4th_wpmodel_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), "wp_model.rds")
}

.games_nfl4th <- function() {
  if (probably_cran() && !force_cache()) {
    return(get_games_file())
  }
  if (!file.exists(nfl4th_games_path())) {
    saveRDS(get_games_file(), nfl4th_games_path())
  }
  readRDS(nfl4th_games_path())
}

fd_model <- function() {
  cached_model("fd")
}

wp_model <- function() {
  cached_model("wp")
}

cached_model <- function(type = c("wp", "fd")) {
  type <- arg_match(type)
  model_load <- switch(
    type,
    "wp" = load_wp_model,
    "fd" = load_fd_model
  )
  # a.) we don't want to handle any sort of cache
  # just return the model
  if (probably_cran() && !force_cache()) {
    model <- model_load()
    return(
      xgboost::xgb.load.raw(model)
    )
  }

  # b.) we want a cache
  # if the model is cached, it is saved under this file path
  model_path <- switch(
    type,
    "wp" = nfl4th_wpmodel_path(),
    "fd" = nfl4th_fdmodel_path()
  )

  if (!file.exists(model_path)) {
    # the file doesn't exist -> file isn't cached yet
    # load and save it and return the parsed model
    model <- model_load()
    saveRDS(model, model_path)
    return(
      xgboost::xgb.load.raw(model)
    )
  } else {
    # the file exists. read it.
    model <- readRDS(model_path)
    # nfl4th v 1.0.5 introduced new raw model formats that are incompatible
    # with previous formats. We need to check the format of the cached file
    # and clear it in case it's outdated.
    if (is.raw(model)) {
      return(
        xgboost::xgb.load.raw(model)
      )
    } else {
      # file in cache has old format -> remove it and call the whole thing
      # recursively
      file.remove(model_path)
      # this time the cache will be empty and cached_model() will load it fresh
      return(
        cached_model(type = type)
      )
    }
  }
}

#' Reset nfl4th Package Cache
#'
#' @param type One of `"games"` (the default), `"fd_model"`, or `"all"`.
#'   `"games"` will remove an internally used games file.
#'   `"fd_model"` will remove the nfl4th 4th down model (only necessary in the
#'   unlikely case of a model update).
#'   `"wp_model"` will remove the nfl4th win probability model (only necessary in the
#'   unlikely case of a model update).
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
