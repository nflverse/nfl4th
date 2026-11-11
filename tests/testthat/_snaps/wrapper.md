# add_4th_probs works

    Code
      knitr::kable(nfl4th::make_table_data(nfl4th::add_4th_probs(one_play)), digits = 2)
    Message
      i Computing probabilities for 1 plays. . .
    Output
      
      
      |choice             | choice_prob| success_prob| fail_wp| success_wp|
      |:------------------|-----------:|------------:|-------:|----------:|
      |Go for it          |       15.53|        36.34|    6.07|      32.10|
      |Field goal attempt |        9.72|        97.65|    4.68|       9.85|
      |Punt               |          NA|           NA|      NA|         NA|

# add_2pt_probs works

    Code
      knitr::kable(nfl4th::make_2pt_table_data(nfl4th::add_2pt_probs(another_play)),
      digits = 2)
    Message
      i Computing probabilities for  1 plays. . .
    Output
      
      
      |choice   | choice_prob| success_prob| fail_wp| success_wp|
      |:--------|-----------:|------------:|-------:|----------:|
      |Go for 2 |       32.10|        54.49|   20.35|      41.91|
      |Kick XP  |       25.86|        93.31|   20.35|      26.25|

