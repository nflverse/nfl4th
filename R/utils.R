# loads required data if not in package environment
# replaces loading .onLoad
init_nfl4th <- function() {
  pkg_env <- ls("package:nfl4th")
  env <- loadNamespace("nfl4th")

  if (!".games_nfl4th" %in% pkg_env){
    .games_nfl4th <- get_games_file()
    unlockBinding(".games_nfl4th", env)
    assign(".games_nfl4th", .games_nfl4th, envir = asNamespace("nfl4th"))
    lockBinding(".games_nfl4th", env)
  }

  if (!"fd_model" %in% pkg_env){
    fd_model <- load_fd_model()
    unlockBinding("fd_model", env)
    assign("fd_model", fd_model, envir = asNamespace("nfl4th"))
    lockBinding("fd_model", env)
  }
}

.games_nfl4th <- fd_model <- NULL
