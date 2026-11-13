# nfl4th (development version)

* New `bootstrap` argument to `add_4th_probs()`. With `bootstrap = TRUE` every play is also valued under 100 bootstrap versions of nfl4th's win probability and go for it models, each pair refit on a history resampled by game, which shows how much a recommendation depends on the particular games the models were fit on. Added columns: `boot_pct` (percent of the 101 draws, the shipped fit included, whose best option is the shipped fit's), `boot_go_pct` (percent in which going beats the better kick), `go_boost_lo` and `go_boost_hi` (the 10th and 90th percentiles, an 80% interval), `boot_median_draw` (the draw at the median `go_boost`, whose numbers form one coherent fit), and list columns with every probability in each draw. The draws (38 MB) are downloaded once to the package cache; `nfl4th_clear_cache("bootstrap")` removes them. One play takes about half a second. `nfl4th_start_workers()` and `nfl4th_stop_workers()` keep the draws in worker processes, which brings that to 0.2-0.4 seconds. (#64)
* nfl4th no longer depends on nflfastR: the parts it uses (`vegas_wp` for the go for it model's late flag, the overtime model and nflfastR's field goal table) are copied in. It also no longer depends on dplyr, janitor, mgcv, purrr, rlang, stringr or tidyr. (#64)
* `add_4th_probs()` is about four times as fast (2026 weeks 1-9: 4.4 seconds instead of 17.9), with the same results. (#64)
* `get_4th_plays()` no longer returns no plays when an unrelated warning is raised while reading a game: the warning is shown and the plays are returned (a warning before an invalid game ID was detected also made it fail with "object 'warn' not found"). Team abbreviations that ESPN or older games use (WSH, LAR, OAK, SD, STL, JAC, ...) are now standardized the same way in the team with the ball, the team that called a timeout and the side of the field. (#64)
* Rebuilt nfl4th's win probability model for the 1st down states that follow a 4th down decision, following the recipe of nflfastR's new `vegas_wp` model: a probit model of the final margin with corrections from monotone constrained trees, more trees late in games than early, and a cap so that a team is never worse off after scoring a touchdown. Win probability is now smooth in the clock and in field position, rises as the offense gets closer to the end zone, and uses both teams' timeouts (the old model only saw the home team's). At the end of a half a possession is valued by the field goals still in play, as in nflfastR's `vegas_wp`: the value of having the ball fades by the time needed to get into field goal range rather than to the end zone, timeouts count at the end of the 1st half too, and win probability is at least the value of kicking a field goal on the snap. At the end of the game it uses the rules of nflfastR's `vegas_wp` (nflfastR#625):
  - In the last 4 minutes, win probability is averaged over 16 seconds and 2 yards either side.
  - A team ahead that can kneel out the clock wins (nflfastR uses 0.9998), moving back to the model as it gets 10 to 40 seconds short of that. This is now the only kneel rule: the states after a 4th down decision used a separate, cruder one, which also valued a punting team that recovered a muff, or was still ahead after a return touchdown, as losing when it could kneel out the clock.
  - A team behind deep in its own half moves toward 0 over the last 20 seconds.
  - Tied in the last 10 seconds of regulation, win probability moves to the value of the best option left at 0:00: a field goal from the spot (a miss goes to overtime), otherwise overtime at the coin toss.
  - A field goal that ends the game is valued by the result, with a tie valued by nflfastR's overtime model at the coin toss.

  Out of sample (season grouped cross validation), these lower log loss of nfl4th's own model in the last 2 minutes by 0.0013 (se 0.0003). nfl4th now uses this model alone in regulation instead of averaging it with nflfastR's `vegas_wp`: with the same data, recipe and features the two are nearly the same model, the average was no better out of sample, and it doubled the cost of a prediction. On 2022-2026 1st downs, which none of the models compared were trained on, log loss of what nfl4th returns drops from 0.4651 (master's average of the two old models) to 0.4639. (#64)
* Rebuilt the go for it model, which gives the distribution of yards gained when going for it. The old model (a 76 class xgboost classifier trained on 2014-2019) jumped by several points of conversion probability from one yard line to the next and underestimated conversion on recent 4th downs (52% predicted vs 57% actual in 2020-2026, up to 9 points too low on 4th and 11+). The new model is built from smooth parts fit on 2001-2026 3rd and 4th downs: conversion, a touchdown given a conversion, where a conversion that doesn't score ends, and where a failed attempt ends. Conversion depends on distance, field position (including goal to go), team strength (spread and total) and the era (2001-2019 and 2020 on, with the latest era covering future seasons, see `get_fd_era()`); 4th downs have their own distance curve, the era effect applies to 4th downs and depends on distance, and on 4th downs the spread matters at short distances but hardly at long ones. Late and desperate 4th downs (the last 10 minutes of the 4th quarter or overtime while trailing, or a win probability below 15%) get their own conversion curve by distance: most 4th and long attempts come in those situations and convert more often than 4th and long otherwise (4th and 8-10: 36% vs 28%), so a single curve overstated conversion on 4th and long in a normal situation. Interceptions and lost fumbles count as failed attempts. Defensive penalties that give a first down count as conversions also when no run or pass is recorded (offside, for example), but encroachment and neutral zone infractions are left out: the offense may only have been trying to draw the defense offside. Failed attempts no longer end behind the offense's own goal line. (#64)
* The win probability and go for it models are trained on the 2001-2002 Jacksonville home games and 2011_13_DET_NO as rebuilt by nflfastR 6.0.0 (`data-raw/repair_games.R`), because their running score (and so `score_differential` and `vegas_wp`) is wrong in the released play-by-play (nflfastR#625). (#64)
* nfl4th recommends going for it more often: on 2026 weeks 1-9 (regulation), going is recommended on 49% of 4th downs instead of 43%. It recommends going more often on 4th and 2-6, where the go for it model's conversion probability is higher (and better calibrated), and late in games (a punt that lets the other team kneel out the clock is now worth nothing), and less often on 4th and 7+ in normal situations. (#64)
* Rebuilt the punt model. The old one (a kernel density of 2010-2019 punts, read off at the landing spots seen from each yard line, with block, return touchdown and muff rates in 10 yard bins) moved the receiving team's average start by up to 2.5 yards from one line of scrimmage to the next, which flipped close calls back and forth along the field. The new one is smooth in the line of scrimmage: where the receiving team's first snap is (a discrete hazard GAM with its own term for touchbacks, including blocked punts and penalties on the return), a return touchdown, and a muff the punting team recovers, fit on the last 8 seasons, when net punts are about 3 yards longer than in 2010-2013. (#64)
* The 2 point conversion probability now comes from the go for it model, as a 4th and goal from the 2 with the game's spread and total, in its latest era for every season (2 point tries convert about as often in every era: 48.1% in 2001-2019, 49.1% since 2020). It replaces a separate xgboost model trained on 2010-2019 tries. On 2020-2026 tries from the 2, log loss is 0.6916, against 0.6955 for the old model and 0.6932 for the 2001-2019 average. (#64)
* The win probability and go for it models now ship with the package instead of being downloaded and cached. (#64)
* Overtime decisions. `add_4th_probs()`, `add_2pt_probs()` and `get_4th_plays()` now handle overtime plays, valued with nflfastR's overtime win probability model and the overtime rules of the game: sudden death, modified sudden death (a first possession field goal lets the other team answer), and both teams getting the ball (postseason since 2022, regular season since 2025), including ties at the end of regular season overtime. Supply `ot_possession` (1, 2, or 3 for later possessions) for hand-made plays; it is counted from the plays for nflfastR and ESPN data. (#64)
* The receiving team's first snap after a kickoff is now the average first snap under each era's kickoff rules: own 22 (2011-2015), 25 (2016-2023), 30 (2024) and 31 (2025+), the same spots nflfastR's win probability model uses. (#64)
* After a 4th down conversion that doesn't score, the 1st down is now valued at the offense's next snap instead of 6 seconds after the snap. It uses the typical gap after 3rd and 4th down conversions since 2016, by quarter, time left in the half and score: about 37 seconds away from the end of a half, as little as 6 to 10 seconds in the last seconds of a half, with a dip just before the 2 minute warning. A supplied `runoff` used to be ignored: the clock was clamped once per play with `max()`, which undid it, so the extra runoff `get_4th_plays()` gave teams leading late never applied. `runoff` now sets the seconds that run off after the 6 of the play, and `get_4th_plays()` uses the typical gap. Late in a half the clock depends on timeouts: a team with one can stop the clock right after the play (about 6 seconds plus 0.1 per yard gained run off, and a timeout is spent), and the better of using it or not is taken; without one, the clock often runs out before another snap (84% of conversions in the last 7 seconds, 65% at 7-10, 44% at 10-15), and those conversions are valued as that mix. A state at 0:00 of the 4th quarter is now the final result (a tie goes to overtime at the coin toss) instead of a 1st down that could still kick a field goal. (#64)
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
