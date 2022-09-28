.games_nfl4th <- function(){
  if (!".games_nfl4th" %in% ls(envir = .nfl4th_env)){
    .games_nfl4th <- get_games_file()
    assign(".games_nfl4th", .games_nfl4th, envir = .nfl4th_env)
  }
  get(".games_nfl4th", envir = .nfl4th_env)
}

fd_model <- function(){
  if (!"fd_model" %in% ls(envir = .nfl4th_env)){
    fd_model <- load_fd_model()
    assign("fd_model", fd_model, envir = .nfl4th_env)
  }
  get("fd_model", envir = .nfl4th_env)
}
