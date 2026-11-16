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

The original data frame plus the following columns:

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

## Details

Overtime plays (`qtr > 4`) are valued with the overtime rules of the
game (sudden death, modified sudden death, or both teams getting the
ball) using nflfastR's overtime win probability model.
`quarter_seconds_remaining` is then the time left in the overtime
period. Add `ot_possession` (1 for the first possession of overtime, 2
for the second, 3 for any later possession) to tell which possession it
is. Without it, a tied game is assumed to be in the first possession and
a team that trails in the second. `nflfastR` data doesn't need it, the
possessions are counted from the plays.

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
try({# to avoid cran issues
probs <- nfl4th::add_2pt_probs(play)

tibble::glimpse(probs)
})
#> ℹ Computing probabilities for  1 plays. . .
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
#> $ wp_0                       <dbl> 0.1827202
#> $ wp_1                       <dbl> 0.2516818
#> $ wp_2                       <dbl> 0.4182712
#> $ conv_1pt                   <dbl> 0.9330643
#> $ conv_2pt                   <dbl> 0.546151
#> $ wp_go1                     <dbl> 0.2470658
#> $ wp_go2                     <dbl> 0.3113666
# }
```
