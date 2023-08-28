# nfl4th 1.0.4

* Create package cache directory with `tools::R_user_dir()` because CRAN doesn't like `rappdirs::user_cache_dir()`

# nfl4th 1.0.3

* Re-export xgboost models to get rid of annoying warning message
* Tweak how close to end of game it will calculate probabilities
* Required data isn't loaded directly with the package. Instead the download is triggered when necessary and cached in a local package cache. The cache can be cleared with the new function `nfl4th_clear_cache()`. The package resets the cache partly when loaded. This can be prevented with `options(nfl4th.keep_games = TRUE)`.
* Update win probability model. Instead of only using `nflfastR`, it stacks `nflfastR` with another model
* Fixup cache paths.
* Remove tidyverse from Suggests

# nfl4th 1.0.2

* Nothing changed. Forced update to documentation to not get kicked off CRAN

# nfl4th 1.0.1

* Fix for aborted plays on punts being called going for it
* Re-categorized some plays as unknown (i.e., `NA`) `go`: False Start or defensive encroachment along with being lined up to go for it (run formation or pass formation)
* Added `fast` argument to `load_4th_pbp()` which allows for loading pre-computed `go_boost` rather than needing to calculate it

# nfl4th 1.0.0

* Initial public release

# nfl4th 0.0.0.9000

* Release as package
* Fixes with touchdowns. Instead of granting 7 points, assumes teams choose best option
between PAT or 2pt and give pre-conversion attempt WP accordingly
* Fix for punt WP at the end of first half being too high
* Add logic for TD/FG decision on the last play of the first half
* Add possibility for muffed punt
* Reduce field goal chances on very long field goals
