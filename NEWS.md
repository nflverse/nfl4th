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
