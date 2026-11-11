# add_4th_probs works

    Code
      knitr::kable(nfl4th::make_table_data(nfl4th::add_4th_probs(one_play)), digits = 2)
    Message
      i Computing probabilities for 1 plays. . .
    Output
      
      
      |choice             | choice_prob| success_prob| fail_wp| success_wp|
      |:------------------|-----------:|------------:|-------:|----------:|
      |Go for it          |       12.64|        36.34|    2.35|      30.67|
      |Field goal attempt |        6.34|        97.65|    1.64|       6.45|
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
      |Go for 2 |       30.67|        54.49|   17.46|      41.71|
      |Kick XP  |       23.23|        93.31|   17.46|      23.64|

