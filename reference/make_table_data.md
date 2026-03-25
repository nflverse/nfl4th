# Get 4th down decision probabilities

Get a table with the probabilities on 4th down.

## Usage

``` r
make_table_data(probs)
```

## Arguments

- probs:

  A data frame consisting of one play that has had
  [`add_4th_probs()`](https://www.nfl4th.com/reference/add_4th_probs.md)
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
    quarter_seconds_remaining = 129,
    ydstogo = 8,
    yardline_100 = 8,
    score_differential = -8,

    home_opening_kickoff = 0,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
try({# to avoid cran issues
probs <- nfl4th::add_4th_probs(play)
nfl4th::make_table_data(probs)
})
#> ℹ Computing probabilities for 1 plays. . .
#> # A tibble: 3 × 5
#>   choice             choice_prob success_prob fail_wp success_wp
#>   <chr>                    <dbl>        <dbl>   <dbl>      <dbl>
#> 1 Go for it                 13.7         32.7    3.58       34.5
#> 2 Field goal attempt        10.0         98.0    3.07       10.2
#> 3 Punt                      NA           NA     NA          NA  
# }
```
