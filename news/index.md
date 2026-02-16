# Changelog

## nfl4th (development version)

- The internal cache is now aware of changed model formats and forces a
  model update for compatibility with xgboost (\>= v3).

## nfl4th 1.0.5

CRAN release: 2026-02-10

- Stop letting roof break things
- New location for pre-computed numbers
- Re-work field goal model. Takes into account improvements in accuracy
  (especially on long kicks) and gives probability \> 0 on kicks of over
  70 yards
- Update models and internals to be compatible with newer xgboost (\>=
  v3) versions. ([\#41](https://github.com/nflverse/nfl4th/issues/41))
- nfl4th now requires R 4.1 to allow the package to use R’s native pipe
  `|>` operator. This follows the [Tidyverse R version support
  rules](https://tidyverse.org/blog/2019/04/r-version-support/).
  ([\#43](https://github.com/nflverse/nfl4th/issues/43))

## nfl4th 1.0.4

CRAN release: 2023-08-21

- Create package cache directory with
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) because
  CRAN doesn’t like
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)

## nfl4th 1.0.3

CRAN release: 2023-08-10

- Re-export xgboost models to get rid of annoying warning message
- Tweak how close to end of game it will calculate probabilities
- Required data isn’t loaded directly with the package. Instead the
  download is triggered when necessary and cached in a local package
  cache. The cache can be cleared with the new function
  [`nfl4th_clear_cache()`](https://www.nfl4th.com/reference/nfl4th_clear_cache.md).
  The package resets the cache partly when loaded. This can be prevented
  with `options(nfl4th.keep_games = TRUE)`.
- Update win probability model. Instead of only using `nflfastR`, it
  stacks `nflfastR` with another model
- Fixup cache paths.
- Remove tidyverse from Suggests

## nfl4th 1.0.2

CRAN release: 2022-08-11

- Nothing changed. Forced update to documentation to not get kicked off
  CRAN

## nfl4th 1.0.1

CRAN release: 2021-10-16

- Fix for aborted plays on punts being called going for it
- Re-categorized some plays as unknown (i.e., `NA`) `go`: False Start or
  defensive encroachment along with being lined up to go for it (run
  formation or pass formation)
- Added `fast` argument to
  [`load_4th_pbp()`](https://www.nfl4th.com/reference/load_4th_pbp.md)
  which allows for loading pre-computed `go_boost` rather than needing
  to calculate it

## nfl4th 1.0.0

CRAN release: 2021-03-17

- Initial public release

## nfl4th 0.0.0.9000

- Release as package
- Fixes with touchdowns. Instead of granting 7 points, assumes teams
  choose best option between PAT or 2pt and give pre-conversion attempt
  WP accordingly
- Fix for punt WP at the end of first half being too high
- Add logic for TD/FG decision on the last play of the first half
- Add possibility for muffed punt
- Reduce field goal chances on very long field goals
