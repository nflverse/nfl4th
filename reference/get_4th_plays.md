# Get 4th down plays from a game

Get 4th down plays from a game.

## Usage

``` r
get_4th_plays(gid)
```

## Arguments

- gid:

  A game to get 4th down decisions of.

## Value

Original data frame Data frame plus the following columns added:

- desc:

  Play description from ESPN.

- type_text:

  Play type text from ESPN.

- index:

  Index number of play from a given game. Useful for tracking plays
  (e.g. for 4th down bot).

- The rest:

  All the columns needed for `add_4th_probs().`

## Details

Obtains a data frame that can be used with
[`add_4th_probs()`](https://www.nfl4th.com/reference/add_4th_probs.md).
The following columns must be present:

- game_id : game ID in nflfastR format (eg '2020_20_TB_GB')

## Examples

``` r
# \donttest{
try({# to avoid cran issues
plays <- nfl4th::get_4th_plays('2020_20_TB_GB')

dplyr::glimpse(plays)
})
#> Rows: 9
#> Columns: 23
#> $ game_id                    <chr> "2020_20_TB_GB", "2020_20_TB_GB", "2020_20_…
#> $ espn_id                    <chr> "401220402", "401220402", "401220402", "401…
#> $ play_id                    <chr> "401220402409", "401220402628", "4012204021…
#> $ desc                       <chr> "(7:41) J.Scott punts 38 yards to TB 27, Ce…
#> $ type                       <chr> "post", "post", "post", "post", "post", "po…
#> $ qtr                        <int> 1, 1, 2, 2, 2, 4, 4, 4, 4
#> $ quarter_seconds_remaining  <dbl> 461, 176, 299, 137, 13, 651, 500, 282, 125
#> $ posteam                    <chr> "GB", "TB", "GB", "TB", "TB", "GB", "GB", "…
#> $ away_team                  <chr> "TB", "TB", "TB", "TB", "TB", "TB", "TB", "…
#> $ home_team                  <chr> "GB", "GB", "GB", "GB", "GB", "GB", "GB", "…
#> $ yardline                   <chr> "GB 35", "TB 44", "TB 6", "GB 47", "GB 45",…
#> $ yardline_100               <int> 65, 56, 6, 47, 45, 86, 76, 28, 8
#> $ ydstogo                    <int> 17, 15, 6, 9, 4, 15, 10, 8, 8
#> $ posteam_timeouts_remaining <dbl> 3, 3, 2, 3, 1, 3, 3, 3, 3
#> $ defteam_timeouts_remaining <dbl> 3, 3, 3, 2, 1, 3, 3, 3, 3
#> $ home_opening_kickoff       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ score_differential         <int> -7, 7, -7, 4, 4, -5, -5, 5, -8
#> $ runoff                     <dbl> 0, 0, 0, 0, 0, 0, 0, 40, 0
#> $ home_score                 <int> 0, 0, 7, 10, 10, 23, 23, 23, 23
#> $ away_score                 <int> 7, 7, 14, 14, 14, 28, 28, 28, 31
#> $ type_text                  <chr> "Punt", "Punt", "Field Goal Good", "Punt", …
#> $ season                     <int> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2…
#> $ index                      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9
# }
```
