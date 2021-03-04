
add_4th_probs <- function(df) {

  if (!"home_opening_kickoff" %in% names(df)) {
    message("home_opening_kickoff not found. Assuming an nflfastR df and doing necessary cleaning . . .")
    df <- df %>%
      prepare_nflfastr_data()
  }

  message("Performing final preparation . . .")
  df <- df %>%
    prepare_df()

  message(glue::glue("Computing probabilities for  {nrow(df)} plays. . ."))
  df <- df %>%
    add_probs()

  return(df)

}
