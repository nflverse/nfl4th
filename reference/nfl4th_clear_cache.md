# Reset nfl4th Package Cache

Reset nfl4th Package Cache

## Usage

``` r
nfl4th_clear_cache(type = c("games", "fd_model", "wp_model", "all"))
```

## Arguments

- type:

  One of `"games"` (the default), `"fd_model"`, or `"all"`. `"games"`
  will remove an internally used games file. `"fd_model"` will remove
  the nfl4th 4th down model (only necessary in the unlikely case of a
  model update). `"wp_model"` will remove the nfl4th win probability
  model (only necessary in the unlikely case of a model update). `"all"`
  will remove all of the above.

## Value

Returns `TRUE` invisibly if cache has been cleared.

## Examples

``` r
nfl4th_clear_cache()
```
