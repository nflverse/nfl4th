# add_4th_probs works

    Code
      knitr::kable(nfl4th::make_table_data(nfl4th::add_4th_probs(one_play)), digits = 2)
    Message
      i Computing probabilities for 1 plays. . .
    Output
      
      
      |choice             | choice_prob| success_prob| fail_wp| success_wp|
      |:------------------|-----------:|------------:|-------:|----------:|
      |Go for it          |       13.68|        32.70|    3.58|      34.46|
      |Field goal attempt |       10.02|        98.05|    3.07|      10.16|
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
      |Go for 2 |       34.46|        58.89|   21.19|      43.71|
      |Kick XP  |       26.85|        94.07|   21.19|      27.21|

