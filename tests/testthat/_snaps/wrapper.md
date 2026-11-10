# add_4th_probs works

    Code
      knitr::kable(nfl4th::make_table_data(nfl4th::add_4th_probs(one_play)), digits = 2)
    Message
      i Computing probabilities for 1 plays. . .
    Output
      
      
      |choice             | choice_prob| success_prob| fail_wp| success_wp|
      |:------------------|-----------:|------------:|-------:|----------:|
      |Go for it          |       17.19|        40.22|    6.03|      33.78|
      |Field goal attempt |       10.12|        97.65|    4.67|      10.25|
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
      |Go for 2 |       33.78|        58.89|   20.92|      42.76|
      |Kick XP  |       27.22|        93.31|   20.92|      27.67|

