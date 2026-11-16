# Get 4th down decision probabilities

Get various probabilities associated with each option on 4th downs (go
for it, kick field goal, punt).

## Usage

``` r
add_4th_probs(df, bootstrap = FALSE)
```

## Arguments

- df:

  A data frame of decisions to be computed for.

- bootstrap:

  If `TRUE`, also value every play under 200 bootstrap versions of
  nfl4th's win probability and go for it models (see Details). The first
  use downloads them (about 80 MB) to the package cache.

## Value

The original data frame plus the following columns:

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

With `bootstrap = TRUE`, also:

- boot_pct:

  Percent of the 201 draws whose best option (go, field goal or punt) is
  the one with the shipped fit.

- boot_go_pct:

  Percent of the draws in which going beats the better kick (go_boost \>
  0). The recommendation itself is the sign of go_boost; this says how
  often the draws agree that going is better.

- go_boost_lo, go_boost_hi:

  10th and 90th percentile of go_boost across the draws, an 80%
  interval.

- boot_median_draw:

  The draw whose go_boost is the median of the play's draws (1 to 201; 1
  is the shipped fit). Taking every number from that draw gives one
  coherent fit at the median, e.g.
  `go_wp_boot[[i]][boot_median_draw[i]]`.

- go_boost_boot:

  go_boost in each draw (a list column of length 201 vectors, the
  shipped fit first).

- go_wp_boot, fg_wp_boot, punt_wp_boot, first_down_prob_boot,
  wp_fail_boot, wp_succeed_boot, fg_make_prob_boot, miss_fg_wp_boot,
  make_fg_wp_boot:

  Each of those columns in each draw, likewise. Within a draw they fit
  together: go_wp is first_down_prob x wp_succeed + (1 -
  first_down_prob) x wp_fail, and fg_wp likewise. fg_make_prob is the
  same in every draw (the field goal table isn't resampled).

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

A conversion that doesn't score is valued at the offense's next snap, at
the typical gap after a conversion for the quarter, the time left and
the score (about 37 seconds away from the end of a half, as little as 6
to 10 in the last seconds). Add `runoff` to set the seconds that run off
after the 6 of the play instead.

With `bootstrap = TRUE` each play is also valued under 200 bootstrap
versions of the two models nfl4th fits itself, the 1st down win
probability model and the go for it model, each refit on a history
resampled by game. With the shipped fit that makes 201 draws, the
shipped fit first. They show how much a recommendation depends on the
particular games the models were fit on (sampling uncertainty, not every
source of model error). The punt and field goal models are the same in
every draw.

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

tibble::glimpse(probs)
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
#> $ go_boost                   <dbl> 6.078348
#> $ first_down_prob            <dbl> 0.3650103
#> $ wp_fail                    <dbl> 0.02118824
#> $ wp_succeed                 <dbl> 0.3113666
#> $ go_wp                      <dbl> 0.1271063
#> $ fg_make_prob               <dbl> 0.9764518
#> $ miss_fg_wp                 <dbl> 0.01470289
#> $ make_fg_wp                 <dbl> 0.06756774
#> $ fg_wp                      <dbl> 0.06632286
#> $ punt_wp                    <dbl> NA
# }
```
