# Get 2pt decision probabilities

Get various probabilities associated with each option on PATs (go for
it, kick PAT).

## Usage

``` r
add_2pt_probs(df)
```

## Arguments

- df:

  A data frame of decisions to be computed for.

## Value

Original data frame Data frame plus the following columns added:

- wp_0:

  Win probability when scoring 0 points on PAT.

- wp_1:

  Win probability when scoring 1 point on PAT.

- wp_2:

  Win probability when scoring 2 points on PAT.

- conv_1pt:

  Probability of making PAT kick.

- conv_2pt:

  Probability of converting 2-pt attempt.

- wp_go1:

  Win probability associated with going for 1.

- wp_go2:

  Win probability associated with going for 2.

## Examples

``` r
# \donttest{
play <-
  tibble::tibble(
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

probs <- nfl4th::add_2pt_probs(play)
#> ℹ Computing probabilities for  1 plays. . .

dplyr::glimpse(probs)
#> Rows: 1
#> Columns: 18
#> $ home_team                  <chr> "GB"
#> $ away_team                  <chr> "TB"
#> $ posteam                    <chr> "GB"
#> $ type                       <chr> "post"
#> $ season                     <dbl> 2020
#> $ qtr                        <dbl> 4
#> $ quarter_seconds_remaining  <dbl> 123
#> $ score_differential         <dbl> -2
#> $ home_opening_kickoff       <dbl> 0
#> $ posteam_timeouts_remaining <dbl> 3
#> $ defteam_timeouts_remaining <dbl> 3
#> $ wp_0                       <dbl> 0.2119228
#> $ wp_1                       <dbl> 0.2721162
#> $ wp_2                       <dbl> 0.4371383
#> $ conv_1pt                   <dbl> 0.9407482
#> $ conv_2pt                   <dbl> 0.588913
#> $ wp_go1                     <dbl> 0.2685497
#> $ wp_go2                     <dbl> 0.3445551
# }
```
