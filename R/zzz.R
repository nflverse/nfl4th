.onLoad <- function(libname, pkgname) {
  .games_nfl4th <- get_games_file()
  fd_model <- load_fd_model()
  assign(".games_nfl4th", .games_nfl4th, envir = parent.env(environment()))
  assign("fd_model", fd_model, envir = parent.env(environment()))
}
