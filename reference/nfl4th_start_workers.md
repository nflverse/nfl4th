# Keep the bootstrap models in worker processes

`add_4th_probs(bootstrap = TRUE)` values every play under 100 bootstrap
draws of nfl4th's models. `nfl4th_start_workers()` starts R processes
that each load a share of the draws once and keep them in memory; while
they run, each play's draws are evaluated in them in parallel, which
cuts the time per play (with 11 workers, about 0.3 seconds for one play
early in a game and 1 second in the last 2 minutes, against about 0.6
and 2 seconds without). `nfl4th_stop_workers()` stops them. They also
stop when the R session ends.

The workers are separate processes
([`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html)),
so they work on every platform; starting them takes a few seconds.
Without workers, everything runs in the main process.

## Usage

``` r
nfl4th_start_workers(n = max(1L, parallel::detectCores() - 1L))

nfl4th_stop_workers()
```

## Arguments

- n:

  Number of worker processes.

## Value

The number of workers, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
nfl4th_start_workers(4)
# ... add_4th_probs(plays, bootstrap = TRUE) ...
nfl4th_stop_workers()
} # }
```
