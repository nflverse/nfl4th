# Reset nfl4th Package Cache

Reset nfl4th Package Cache

## Usage

``` r
nfl4th_clear_cache(
  type = c("games", "fd_model", "wp_model", "bootstrap", "all")
)
```

## Arguments

- type:

  One of `"games"` (the default), `"fd_model"`, `"wp_model"`,
  `"bootstrap"`, or `"all"`. `"games"` will remove an internally used
  games file. `"bootstrap"` will remove the bootstrap models downloaded
  by `add_4th_probs(bootstrap = TRUE)`. `"fd_model"` and `"wp_model"`
  will remove the go for it and win probability models cached by earlier
  nfl4th versions (both models now ship with the package). `"all"` will
  remove all of the above.

## Value

Returns `TRUE` invisibly if cache has been cleared.

## Examples

``` r
nfl4th_clear_cache()
```
