# Get 4th down decision probabilities

Get various probabilities associated with each option on 4th downs (go
for it, kick field goal, punt).

## Usage

``` r
add_4th_probs(df)
```

## Arguments

- df:

  A data frame of decisions to be computed for.

## Value

Original data frame Data frame plus the following columns added:

- go_boost:

  Gain (or loss) in win prob associated with choosing to go for it
  (percentage points).

- first_down_prob:

  Probability of earning a first down if going for it on 4th down.

- wp_fail:

  Win probability in the event of a failed 4th down attempt.

- wp_succeed:

  Win probability in the event of a successful 4th down attempt.

- go_wp:

  Average win probability when going for it on 4th down.

- fg_make_prob:

  Probability of making field goal.

- miss_fg_wp:

  Win probability in the event of a missed field goal.

- make_fg_wp:

  Win probability in the event of a made field goal.

- fg_wp:

  Average win probability when attempting field goal.

- punt_wp:

  Average win probability when punting.

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

dplyr::glimpse(probs)
})
#> ℹ Computing probabilities for 1 plays. . .
#> Rows: 1
#> Columns: 23
#> $ home_team                  <chr> "GB"
#> $ away_team                  <chr> "TB"
#> $ posteam                    <chr> "GB"
#> $ type                       <chr> "post"
#> $ season                     <dbl> 2020
#> $ qtr                        <dbl> 4
#> $ quarter_seconds_remaining  <dbl> 129
#> $ ydstogo                    <dbl> 8
#> $ yardline_100               <dbl> 8
#> $ score_differential         <dbl> -8
#> $ home_opening_kickoff       <dbl> 0
#> $ posteam_timeouts_remaining <dbl> 3
#> $ defteam_timeouts_remaining <dbl> 3
#> $ go_boost                   <dbl> 3.650767
#> $ first_down_prob            <dbl> 0.3269784
#> $ wp_fail                    <dbl> 0.03575731
#> $ wp_succeed                 <dbl> 0.3446303
#> $ go_wp                      <dbl> 0.1367521
#> $ fg_make_prob               <dbl> 0.9804959
#> $ miss_fg_wp                 <dbl> 0.03068082
#> $ make_fg_wp                 <dbl> 0.1016282
#> $ fg_wp                      <dbl> 0.1002445
#> $ punt_wp                    <dbl> NA
# }
```
