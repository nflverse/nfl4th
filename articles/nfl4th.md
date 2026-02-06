# Get Started with nfl4th

First load the packages and some data.
[`load_4th_pbp()`](https://www.nfl4th.com/reference/load_4th_pbp.md)
loads `nflfastR` data and computes 4th down probabilities (depending on
your computer, this may take up to a minute or two per season).

``` r
library(nfl4th)
library(tidyverse)
library(gt)

data <- nfl4th::load_4th_pbp(2020)
```

## Easy mode: using nflfastR data

Here’s what the data obtained using
[`load_4th_pbp()`](https://www.nfl4th.com/reference/load_4th_pbp.md)
looks like:

``` r
data |>
  dplyr::filter(!is.na(go_boost)) |>
  utils::head(10) |>
  dplyr::select(
    posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) |>
  knitr::kable(digits = 2)
```

| posteam | ydstogo | yardline_100 | go_boost | first_down_prob | wp_fail | wp_succeed | go_wp | fg_make_prob | miss_fg_wp | make_fg_wp | fg_wp | punt_wp |
|:--------|--------:|-------------:|---------:|----------------:|--------:|-----------:|------:|-------------:|-----------:|-----------:|------:|--------:|
| SF      |       3 |           34 |     2.03 |            0.55 |    0.67 |       0.78 |  0.73 |         0.70 |       0.67 |       0.73 |  0.71 |    0.71 |
| ARI     |      10 |           65 |    -1.89 |            0.27 |    0.20 |       0.31 |  0.23 |         0.00 |       0.19 |       0.29 |  0.19 |    0.25 |
| ARI     |       7 |           72 |    -0.21 |            0.38 |    0.08 |       0.14 |  0.10 |         0.00 |       0.08 |       0.15 |  0.08 |    0.11 |
| SF      |       5 |           64 |     0.39 |            0.48 |    0.84 |       0.91 |  0.88 |         0.00 |       0.83 |       0.92 |  0.83 |    0.87 |
| SF      |       3 |           68 |     0.60 |            0.55 |    0.64 |       0.79 |  0.72 |         0.00 |       0.62 |       0.79 |  0.62 |    0.71 |
| ARI     |       9 |           77 |    -1.03 |            0.30 |    0.17 |       0.29 |  0.21 |         0.00 |       0.17 |       0.28 |  0.17 |    0.22 |
| SF      |       1 |            1 |     3.34 |            0.63 |    0.77 |       0.88 |  0.84 |         0.99 |       0.75 |       0.81 |  0.81 |      NA |
| ARI     |       5 |           34 |    -0.18 |            0.45 |    0.20 |       0.37 |  0.28 |         0.70 |       0.20 |       0.31 |  0.28 |    0.23 |
| SF      |       9 |           36 |    -1.10 |            0.32 |    0.72 |       0.84 |  0.76 |         0.65 |       0.72 |       0.80 |  0.77 |    0.76 |
| SF      |       2 |            6 |     0.74 |            0.54 |    0.75 |       0.87 |  0.82 |         0.99 |       0.74 |       0.81 |  0.81 |      NA |

Or we can add some filters to look up a certain game:

``` r
data |>
  dplyr::filter(week == 20, posteam == "GB", down == 4) |>
  dplyr::select(
    posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) |>
  knitr::kable(digits = 2)
```

| posteam | ydstogo | yardline_100 | go_boost | first_down_prob | wp_fail | wp_succeed | go_wp | fg_make_prob | miss_fg_wp | make_fg_wp | fg_wp | punt_wp |
|:--------|--------:|-------------:|---------:|----------------:|--------:|-----------:|------:|-------------:|-----------:|-----------:|------:|--------:|
| GB      |      17 |           65 |    -3.26 |            0.15 |    0.32 |       0.47 |  0.34 |         0.00 |       0.30 |       0.46 |  0.30 |    0.37 |
| GB      |       6 |            6 |    -1.72 |            0.31 |    0.35 |       0.58 |  0.42 |         0.99 |       0.33 |       0.44 |  0.44 |      NA |
| GB      |      15 |           86 |    -1.42 |            0.19 |    0.16 |       0.38 |  0.21 |         0.00 |       0.16 |       0.33 |  0.16 |    0.22 |
| GB      |      10 |           76 |     0.83 |            0.32 |    0.15 |       0.37 |  0.22 |         0.00 |       0.14 |       0.31 |  0.14 |    0.21 |
| GB      |       8 |            8 |     3.65 |            0.33 |    0.04 |       0.34 |  0.14 |         0.98 |       0.03 |       0.10 |  0.10 |      NA |

We see the infamous field goal at the bottom.

## Calculations from user input

The below shows the bare minimum amount of information that has to be
fed to `nfl4th` in order to compute 4th down decision recommendations.
The main function on user-input data is
[`add_4th_probs()`](https://www.nfl4th.com/reference/add_4th_probs.md).

The reason teams from a specific game have to be used is that the model
depends on factors such as point spread, team totals, and indoor/outdoor
and the program automatically looks these up so that users don’t have to
provide them.

``` r
one_play <- tibble::tibble(
  
  # things to help find the right game (use "reg" or "post" for type)
  home_team = "GB",
  away_team = "TB",
  posteam = "GB",
  type = "post",
  season = 2020,
  
  # information about the situation
  qtr = 4,
  quarter_seconds_remaining = 129,
  ydstogo = 8,
  yardline_100 = 8,
  score_differential = -8,

  home_opening_kickoff = 0,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3
)

one_play |>
  nfl4th::add_4th_probs() |>
  dplyr::select(
    posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) |>
  knitr::kable(digits = 2)
#> ℹ Computing probabilities for 1 plays. . .
```

| posteam | ydstogo | yardline_100 | go_boost | first_down_prob | wp_fail | wp_succeed | go_wp | fg_make_prob | miss_fg_wp | make_fg_wp | fg_wp | punt_wp |
|:--------|--------:|-------------:|---------:|----------------:|--------:|-----------:|------:|-------------:|-----------:|-----------:|------:|--------:|
| GB      |       8 |            8 |     3.65 |            0.33 |    0.04 |       0.34 |  0.14 |         0.98 |       0.03 |        0.1 |   0.1 |      NA |

Comparing this and the table above, we see the exact same numbers as
expected.

## Make a summary table

Let’s put the play above into a table using the provided function
[`make_table_data()`](https://www.nfl4th.com/reference/make_table_data.md),
which makes it easier to interpret the recommendations for a play. This
function only works with one play at a time since it makes a table using
the results from the play.

``` r
one_play |>
  nfl4th::add_4th_probs() |>
  nfl4th::make_table_data() |>
  knitr::kable(digits = 1)
#> ℹ Computing probabilities for 1 plays. . .
```

| choice             | choice_prob | success_prob | fail_wp | success_wp |
|:-------------------|------------:|-------------:|--------:|-----------:|
| Go for it          |        13.7 |         32.7 |     3.6 |       34.5 |
| Field goal attempt |        10.0 |         98.0 |     3.1 |       10.2 |
| Punt               |          NA |           NA |      NA |         NA |

Looking at the table, the Packers would be expected to have 12.7% win
probability if they had gone for it and 8.9% if they kicked a field
goal. This difference of 3.8 percentage points is [almost exactly the
same as PFF’s 3.5 percentage
points](https://twitter.com/benbbaldwin/status/1354239287299088384) for
the decision.

## Make a summary table for a 2-point decision

`nfl4th` also contains a function to calculate 2-point decisions. Let’s
put in the situation that would have happened if the Packers had scored
a touchdown on the 4th & 8. We don’t need a calculator to know that they
should have gone for two, but let’s practice by putting in the numbers,
assuming that the 4th down play took 6 seconds while resulting in a
touchdown.

``` r
another_play <- tibble::tibble(
  
  # things to help find the right game (use "reg" or "post")
  home_team = "GB",
  away_team = "TB",
  posteam = "GB",
  type = "post",
  season = 2020,
  
  # information about the situation
  qtr = 4,
  quarter_seconds_remaining = 123,
  score_differential = -2,

  home_opening_kickoff = 0,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3
)

another_play |>
  nfl4th::add_2pt_probs() |>
  nfl4th::make_2pt_table_data() |>
  knitr::kable(digits = 1)
#> ℹ Computing probabilities for  1 plays. . .
```

| choice   | choice_prob | success_prob | fail_wp | success_wp |
|:---------|------------:|-------------:|--------:|-----------:|
| Go for 2 |        34.5 |         58.9 |    21.2 |       43.7 |
| Kick XP  |        26.9 |         94.1 |    21.2 |       27.2 |

Note that the go for 2 probability here is identical to the win
probability associated with a successful 4th down conversion above
because the 4th down model assumes that the Packers would go for 2 if
they scored.

## Getting 4th down plays from a live game

`nflfastR` isn’t available for live games and typing all the plays in by
hand is annoying. So how does the 4th down bot work? With thanks to the
ESPN API, which can be accessed using
[`get_4th_plays()`](https://www.nfl4th.com/reference/get_4th_plays.md).

``` r
plays <- get_4th_plays("2020_20_TB_GB") |>
  tail(1)

plays |> 
  select(desc, quarter_seconds_remaining)
#> # A tibble: 1 × 2
#>   desc                             quarter_seconds_remaining
#>   <chr>                                                <dbl>
#> 1 "Mason Crosby 26 Yd Field Goal "                       125

plays |> 
  nfl4th::add_4th_probs() |>
  nfl4th::make_table_data() |>
  knitr::kable(digits = 1)
#> ℹ Computing probabilities for 1 plays. . .
```

| choice             | choice_prob | success_prob | fail_wp | success_wp |
|:-------------------|------------:|-------------:|--------:|-----------:|
| Go for it          |        13.5 |         32.7 |     3.4 |       34.2 |
| Field goal attempt |         9.5 |         98.0 |     3.0 |        9.7 |
| Punt               |          NA |           NA |      NA |         NA |

Note that the probabilities are slightly different here because ESPN has
the wrong value for time remaining (125 seconds instead of 129). This
doesn’t affect a lot of plays but is something to be aware of.
