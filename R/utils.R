# loads required data if not in package environment
# replaces loading .onLoad
init_nfl4th <- function() {
  pkg_env <- ls("package:nfl4th")

  if (!".games_nfl4th" %in% pkg_env){
    .games_nfl4th <- get_games_file()
    assign(".games_nfl4th", .games_nfl4th, envir = parent.env(environment()))
  }

  if (!"fd_model" %in% pkg_env){
    fd_model <- load_fd_model()
    assign("fd_model", fd_model, envir = parent.env(environment()))
  }
}
