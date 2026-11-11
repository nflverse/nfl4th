# nfl4th (development version)

* Rebuilt nfl4th's win probability model for the 1st down states that follow a 4th down decision, following the recipe of nflfastR's new `vegas_wp` model: a probit model of the final margin with corrections from monotone constrained trees, more trees late in games than early, and a cap so that a team is never worse off after scoring a touchdown. Win probability is now smooth in the clock and in field position, rises as the offense gets closer to the end zone, and uses both teams' timeouts (the old model only saw the home team's). At the end of a half a possession is valued by the field goals still in play, as in nflfastR's `vegas_wp`: the value of having the ball fades by the time needed to get into field goal range rather than to the end zone, timeouts count at the end of the 1st half too, and win probability is at least the value of kicking a field goal on the snap. At the end of the game it uses the rules of nflfastR's `vegas_wp` (nflfastR#625):
  - In the last 4 minutes, win probability is averaged over 16 seconds and 2 yards either side.
  - A team ahead that can kneel out the clock is at 0.9998, moving back to the model as it gets 10 to 40 seconds short of that.
  - A team behind deep in its own half moves toward 0 over the last 20 seconds.
  - A field goal that ends the game is valued by the result, with a tie valued by nflfastR's overtime model at the coin toss.

  Out of sample (season grouped cross validation), these lower log loss of nfl4th's own model in the last 2 minutes by 0.0013 (se 0.0003). The model is trained on the 2001-2002 Jacksonville home games and 2011_13_DET_NO as rebuilt by nflfastR 6.0.0, because the running score in the released play-by-play is wrong in those games (nflfastR#625). As before, nfl4th averages this model with nflfastR's `vegas_wp`. On 2022-2026 1st downs, which none of the models compared were trained on, log loss of the average drops from 0.4651 to 0.4640 (nfl4th's own model alone: 0.4660 to 0.4638). (#64)
* Rebuilt the go for it model, which gives the distribution of yards gained when going for it. The old model (a 76 class xgboost classifier trained on 2014-2019) jumped by several points of conversion probability from one yard line to the next and underestimated conversion on recent 4th downs (52% predicted vs 57% actual in 2020-2026, up to 9 points too low on 4th and 11+). The new model is built from smooth parts fit on 2001-2026 3rd and 4th downs: conversion, a touchdown given a conversion, where a conversion that doesn't score ends, and where a failed attempt ends. Conversion depends on distance, field position (including goal to go), team strength (spread and total) and the era (2001-2019 and 2020 on, with the latest era covering future seasons, see `get_fd_era()`); 4th downs have their own distance curve, the era effect applies to 4th downs and depends on distance, and on 4th downs the spread matters at short distances but hardly at long ones. Late and desperate 4th downs (the last 10 minutes of the 4th quarter or overtime while trailing, or a win probability below 15%) get their own conversion curve by distance: most 4th and long attempts come in those situations and convert more often than 4th and long otherwise (4th and 8-10: 36% vs 28%), so a single curve overstated conversion on 4th and long in a normal situation. Interceptions and lost fumbles count as failed attempts. Defensive penalties that give a first down count as conversions also when no run or pass is recorded (offside, for example), but encroachment and neutral zone infractions are left out: the offense may only have been trying to draw the defense offside. Failed attempts no longer end behind the offense's own goal line. (#64)
* nfl4th recommends going for it more often: on 2026 weeks 1-9 (regulation), going is recommended on 48% of 4th downs instead of 42%. It recommends going more often on 4th and 2-6, where the go for it model's conversion probability is higher (and better calibrated), and on 4th and long late in games the offense is losing, and less often on 4th and 7+ otherwise. (#64)
* Rebuilt the punt model. The old one (a kernel density of 2010-2019 punts, read off at the landing spots seen from each yard line, with block, return touchdown and muff rates in 10 yard bins) moved the receiving team's average start by up to 2.5 yards from one line of scrimmage to the next, which flipped close calls back and forth along the field. The new one is smooth in the line of scrimmage: where the receiving team's first snap is (a discrete hazard GAM with its own term for touchbacks, including blocked punts and penalties on the return), a return touchdown, and a muff the punting team recovers, fit on the last 8 seasons, when net punts are about 3 yards longer than in 2010-2013. (#64)
* The 2 point conversion probability now comes from the go for it model, as a 4th and goal from the 2 with the game's spread and total, in its latest era for every season (2 point tries convert about as often in every era: 48.1% in 2001-2019, 49.1% since 2020). It replaces a separate xgboost model trained on 2010-2019 tries. On 2020-2026 tries from the 2, log loss is 0.6916, against 0.6955 for the old model and 0.6932 for the 2001-2019 average. (#64)
* The win probability and go for it models now ship with the package instead of being downloaded and cached. (#64)
* Overtime decisions. `add_4th_probs()`, `add_2pt_probs()` and `get_4th_plays()` now handle overtime plays, valued with nflfastR's overtime win probability model and the overtime rules of the game: sudden death, modified sudden death (a first possession field goal lets the other team answer), and both teams getting the ball (postseason since 2022, regular season since 2025), including ties at the end of regular season overtime. Supply `ot_possession` (1, 2, or 3 for later possessions) for hand-made plays; it is counted from the plays for nflfastR and ESPN data. (#64)
* The receiving team's first snap after a kickoff is now the average first snap under each era's kickoff rules: own 22 (2011-2015), 25 (2016-2023), 30 (2024) and 31 (2025+), the same spots nflfastR's win probability model uses. (#64)
* Field goal make probability is now corrected for selection bias in long attempts. Only teams confident in their kicker try them, so the raw model curve was too generous past about 52 yards; probability from 55 to 61 yards drops by 5 to 10 percentage points. This replaces the linear decay that was anchored on the predicted 58 yard probability. (#63)
* The field goal model's era cutoff moves from 2020 to 2023, and the definition is now shared between the prediction and model-fitting code so the two cannot drift. (#63)
* Field goal distance is now `yardline_100` + 18 instead of + 17: the hold is 8 yards behind the line of scrimmage, and `kick_distance` is `yardline_100` + 18 on 90% of attempts. Past 62 yards, make probability continues along the log odds slope of the curve fit where nearly every team kicks, and fades linearly to zero from 65 to 75 yards. Before, it fell linearly to zero at 71 yards (72 yards in real distance). Since 2023, 64+ yard attempts went 7 of 19. The table is the same as nflfastR's (nflfastR#625). (#64)
* From 2024 on, kickoffs after scores and to start the second half put the receiving team at its own 30 instead of its own 25, to match the new kickoff rules. (#61)
* Fix the punt model's return touchdown and blocked punt rates, which were swapped. (#60)

# nfl4th 1.0.7

* Further improvements to internal cache handling by CRAN request. (#56)

# nfl4th 1.0.6

* The internal cache is now aware of changed model formats and forces a model update for compatibility with xgboost (>= v3). (#53)

Thank you to &#x0040;isaactpetersen, and &#x0040;mrcaseb for their questions, feedback, and contributions towards this release.

# nfl4th 1.0.5

* Stop letting roof break things
* New location for pre-computed numbers
* Re-work field goal model. Takes into account improvements in accuracy (especially on long kicks) and gives probability > 0 on kicks of over 70 yards
* Update models and internals to  be compatible with newer xgboost (>= v3) versions. (#41)
* nfl4th now requires R 4.1 to allow the package to use R's native pipe `|>` operator. This follows the [Tidyverse R version support rules](https://tidyverse.org/blog/2019/04/r-version-support/). (#43)

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
