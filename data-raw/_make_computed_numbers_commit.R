message <- sprintf("Updated using nfl4th version %s", utils::packageVersion("nfl4th"))

git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = FALSE) {
  callr::run("git", c(...),
             echo_cmd = echo_cmd, echo = echo,
             error_on_status = error_on_status
  )
}

git("commit", "-am", message)
