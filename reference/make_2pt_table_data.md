# Get 2pt decision probabilities

Get a table with the probabilities associated with a 2-pt decision.

## Usage

``` r
make_2pt_table_data(probs)
```

## Arguments

- probs:

  A data frame consisting of one play that has had
  [`add_2pt_probs()`](https://www.nfl4th.com/reference/add_2pt_probs.md)
  already run on it.

## Value

A table showing the probabilities associated with each possible choice.

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
nfl4th::make_2pt_table_data(probs)
#> # A tibble: 2 × 5
#>   choice   choice_prob success_prob fail_wp success_wp
#>   <chr>          <dbl>        <dbl>   <dbl>      <dbl>
#> 1 Go for 2        34.5         58.9    21.2       43.7
#> 2 Kick XP         26.9         94.1    21.2       27.2
# }
```
