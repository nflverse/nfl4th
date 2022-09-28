# paths are defined in zzz.R
# these helpers read games or fd_model and save them to a package cache

.games_nfl4th <- function(){
  if (!file.exists(nfl4th_games_path)){
    saveRDS(get_games_file(), nfl4th_games_path)
  }
  readRDS(nfl4th_games_path)
}

fd_model <- function(){
  if (!file.exists(nfl4th_model_path)){
    saveRDS(load_fd_model(), nfl4th_model_path)
  }
  readRDS(nfl4th_model_path)
}

#' Reset nfl4th Package Cache
#'
#' @param type One of `"games"` (the default), `"fd_model"`, or `"all"`.
#'   `"games"` will remove an internally used games file.
#'   `"fd_model"` will remove the nfl4th 4th down model (only necessary in the
#'   unlikely case of a model update).
#'   `"all"` will remove both of the above.
#'
#' @return Returns `TRUE` invisibly if cache has been cleared.
#' @export
#'
#' @examples
#' nfl4th_clear_cache()
nfl4th_clear_cache <- function(type = c("games", "fd_model", "all")){
  type <- rlang::arg_match(type)
  to_delete <- switch (type,
    "games" = nfl4th_games_path,
    "fd_model" = nfl4th_model_path,
    "all" = c(nfl4th_games_path, nfl4th_model_path)
  )
  file.remove(to_delete[file.exists(to_delete)])
  invisible(TRUE)
}
