# Bootstrap draws of the two models nfl4th fits itself: the 1st down win
# probability model and the go for it model. Draw 1 is the shipped fit; the
# others refit both models on histories resampled by game
# (data-raw/_bootstrap_models.R). The punt and field goal tables and nflfastR's
# models are the same in every draw. add_4th_probs(bootstrap = TRUE) values
# every play under each draw.

# The models in use: the shipped fit unless a draw is active (with_draw())
nfl4th_active <- new.env(parent = emptyenv())

active_wp_model <- function() {
  m <- nfl4th_active$wp
  if (is.null(m)) wp_first_down_model else m
}

active_fd_tables <- function() {
  tb <- nfl4th_active$fd
  if (is.null(tb)) fd_tables else tb
}

# evaluate `code` with a draw's models
with_draw <- function(draw, code) {
  old <- list(wp = nfl4th_active$wp, fd = nfl4th_active$fd)
  on.exit({
    nfl4th_active$wp <- old$wp
    nfl4th_active$fd <- old$fd
  })
  nfl4th_active$wp <- draw$wp
  nfl4th_active$fd <- draw$fd
  force(code)
}

# The draws for this version of the models, on the nfl4th_infrastructure
# release. Rebuilding the models means rebuilding the draws under a new name.
bootstrap_file <- "bootstrap_models_20261112.rds"

nfl4th_bootstrap_path <- function() {
  file.path(R_user_dir("nfl4th", "cache"), bootstrap_file)
}

bootstrap_url <- paste0(
  "https://github.com/nflverse/nfl4th/releases/download/",
  "nfl4th_infrastructure/",
  bootstrap_file
)

bootstrap_memo <- new.env(parent = emptyenv())

# The draws: from the file in option nfl4th.bootstrap_models if it is set,
# otherwise downloaded once (about 35 MB) to the package cache. Kept in memory.
bootstrap_models <- function() {
  f <- getOption("nfl4th.bootstrap_models")
  if (is.null(f)) {
    f <- nfl4th_bootstrap_path()
    if (!file.exists(f)) {
      cli::cli_alert_info(
        "Downloading the bootstrap models (about 35 MB, once) . . ."
      )
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      tmp <- paste0(f, ".part")
      curl::curl_download(bootstrap_url, tmp, quiet = TRUE)
      file.rename(tmp, f)
    }
  }
  info <- file.info(f)[c("mtime", "size")]
  if (
    is.null(bootstrap_memo$models) ||
      !identical(bootstrap_memo$file, f) ||
      !identical(bootstrap_memo$info, info)
  ) {
    m <- readRDS(f)
    # draw 1 is the shipped fit: draws built for other models don't fit this
    # version of nfl4th
    first <- m$draws[[1]]
    if (
      !identical(first$wp$trees_raw, wp_first_down_model$trees_raw) ||
        !identical(first$fd, fd_tables)
    ) {
      cli::cli_abort(c(
        "The bootstrap models in {.file {f}} were built for other models.",
        "i" = paste(
          "Update nfl4th, or remove them with",
          "{.code nfl4th_clear_cache(\"bootstrap\")} and try again."
        )
      ))
    }
    bootstrap_memo$models <- m
    bootstrap_memo$file <- f
    bootstrap_memo$info <- info
  }
  bootstrap_memo$models
}

# go_boost as add_4th_probs() computes it, for matrices (plays x draws)
boot_go_boost <- function(go, fg, punt) {
  punt[is.na(punt)] <- 0
  kick <- ifelse(is.na(fg), punt, pmax(fg, punt))
  100 * (go - kick)
}

# the best decision (1 go, 2 field goal, 3 punt) for matrices of WP
boot_decision <- function(go, fg, punt) {
  m <- array(c(go, fg, punt), c(dim(go), 3))
  m[is.na(m)] <- -Inf
  apply(m, c(1, 2), which.max)
}

# The inputs of the WP model (wpfd_tree_matrix(), wpfd_design() and the tree
# schedule read nothing else): a state is these columns
wpfd_inputs <- c(
  "game_seconds_remaining",
  "half_seconds_remaining",
  "score_differential",
  "posteam_spread",
  "home",
  "receive_2h_ko",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# evaluate `code` with hooks set in nfl4th_active (NULL removes one)
with_active <- function(set, code) {
  old <- mget(names(set), envir = nfl4th_active, ifnotfound = list(NULL))
  on.exit(
    for (n in names(old)) {
      assign(n, old[[n]], envir = nfl4th_active)
    }
  )
  for (n in names(set)) {
    assign(n, set[[n]], envir = nfl4th_active)
  }
  force(code)
}

# Every play under every draw. `x` is what add_4th_probs() passes to
# add_probs(). Valuing a play asks the WP model about the same states whatever
# the draw (the go for it tables of every draw have the same outcomes), so:
# 1. the decisions run once with the shipped fit (draw 1), recording the states
#    the WP model is asked about: this is the point estimate,
# 2. each other draw's WP model scores all of them at once (a few xgboost calls
#    per draw instead of a few hundred), and
# 3. the decisions run once more on the plays stacked once per other draw,
#    each copy with its draw's go for it tables and WP looked up from step 2.
# The result is the same as running add_probs() under each draw in turn.
# Returns add_probs(x) (`point`) and the bootstrap columns (`cols`). Plays are
# taken `chunk` at a time: step 3 holds every play once per draw, so a whole
# season at once would need many GB. Each play's values don't depend on the
# other plays in the chunk.
add_boot_probs <- function(x, chunk = 50L) {
  n <- nrow(x)
  if (n > chunk) {
    groups <- split(seq_len(n), ceiling(seq_len(n) / chunk))
    bar <- cli::cli_progress_bar("Bootstrap", total = length(groups))
    parts <- lapply(groups, function(i) {
      r <- add_boot_probs(df_slice(x, i), chunk)
      cli::cli_progress_update(id = bar)
      r
    })
    cli::cli_progress_done(id = bar)
    cols <- lapply(
      stats::setNames(nm = names(parts[[1]]$cols)),
      function(k) do.call(c, lapply(unname(parts), function(p) p$cols[[k]]))
    )
    point <- df_bind_rows(lapply(unname(parts), `[[`, "point"))
    return(list(point = point, cols = cols))
  }
  bm <- bootstrap_models()
  draws <- bm$draws
  n_draws <- length(draws)
  others <- seq_len(n_draws)[-1]

  # 1. the point estimate, recording the states
  rec <- new.env(parent = emptyenv())
  rec$q <- list()
  point <- with_active(
    list(
      logit = function(s) {
        rec$q[[length(rec$q) + 1L]] <- as.data.frame(s)[wpfd_inputs]
        with_active(list(logit = NULL), wpfd_logit(s))
      }
    ),
    add_probs(x)
  )
  q <- vctrs::vec_unique(do.call(vctrs::vec_rbind, rec$q))

  # 2. and 3., in the worker processes if they are running
  xr <- vctrs::new_data_frame(
    df_cols(x)[names(x) %in% decision_cols],
    n = .row_names_info(x, 2L)
  )
  ev <- boot_eval_workers(xr, q)
  if (is.null(ev)) {
    ev <- boot_eval(xr, q, others)
  }
  # plays x draws, the shipped fit first
  m <- lapply(
    stats::setNames(nm = boot_columns),
    function(k) cbind(point[[k]], ev[[k]])
  )

  boost <- boot_go_boost(m$go_wp, m$fg_wp, m$punt_wp)
  dec <- boot_decision(m$go_wp, m$fg_wp, m$punt_wp)
  rows <- function(x) lapply(seq_len(n), function(i) x[i, ])
  qs <- apply(boost, 1, stats::quantile, probs = c(0.1, 0.9), na.rm = TRUE)
  cols <- c(
    list(
      boot_pct = 100 * rowMeans(dec == dec[, 1]),
      boot_go_pct = 100 * rowMeans(boost > 0),
      go_boost_lo = qs[1, ],
      go_boost_hi = qs[2, ],
      boot_median_draw = boot_median_draw(boost),
      go_boost_boot = rows(boost)
    ),
    stats::setNames(lapply(m, rows), paste0(boot_columns, "_boot"))
  )
  list(point = point, cols = cols)
}

# the columns of add_probs() kept for every draw
boot_columns <- c(
  "go_wp",
  "fg_wp",
  "punt_wp",
  "first_down_prob",
  "wp_fail",
  "wp_succeed",
  "fg_make_prob",
  "miss_fg_wp",
  "make_fg_wp"
)

# the draw whose go_boost is the median of the play's draws (with 101 draws,
# the 51st smallest; NA draws aside, the lower one of the middle two when
# there is an even number)
boot_median_draw <- function(boost) {
  apply(boost, 1, function(v) {
    ok <- which(!is.na(v))
    if (length(ok) == 0L) {
      return(NA_integer_)
    }
    ok[order(v[ok], method = "radix")][ceiling(length(ok) / 2)]
  })
}

# Steps 2 and 3 of add_boot_probs() for draws `idx`: plays x draws matrices of
# the boot_columns, the draws in the order of `idx`
boot_eval <- function(x, q, idx) {
  draws <- bootstrap_models()$draws
  n <- nrow(x)
  lg <- matrix(NA_real_, nrow(q), length(draws))
  for (b in idx) {
    lg[, b] <- wpfd_logit(q, draws[[b]]$wp)
  }
  xs <- x[rep(seq_len(n), length(idx)), , drop = FALSE]
  xs$.draw <- rep(idx, each = n)
  lookup <- function(s) {
    s <- as.data.frame(s)
    key <- vctrs::vec_cast(s[wpfd_inputs], q[0, , drop = FALSE])
    i <- vctrs::vec_match(key, q)
    out <- lg[cbind(i, s$.draw)]
    # a state step 1 didn't see (only if the decisions took another path
    # with other values): score it directly
    miss <- which(is.na(i))
    for (b in unique(s$.draw[miss])) {
      j <- miss[s$.draw[miss] == b]
      out[j] <- with_active(
        list(logit = NULL),
        wpfd_logit(s[j, , drop = FALSE], draws[[b]]$wp)
      )
    }
    out
  }
  r <- with_active(
    list(logit = lookup, fd_draws = lapply(draws, `[[`, "fd")),
    add_probs(xs)
  )
  lapply(
    stats::setNames(nm = boot_columns),
    function(k) matrix(r[[k]], n)
  )
}

# ---- worker processes ---------------------------------------------------------

nfl4th_workers <- new.env(parent = emptyenv())

#' Keep the bootstrap models in worker processes
#'
#' @description `add_4th_probs(bootstrap = TRUE)` values every play under 100
#' bootstrap draws of nfl4th's models. `nfl4th_start_workers()` starts R
#' processes that each load a share of the draws once and keep them in memory;
#' while they run, each play's draws are evaluated in them in parallel, which
#' cuts the time per play (about a third of a second for one play in the last
#' minute of a game with 11 workers, against about 0.85 seconds without).
#' `nfl4th_stop_workers()` stops them. They also stop when the R session ends.
#'
#' The workers are separate processes (`parallel::makePSOCKcluster()`), so they
#' work on every platform; starting them takes a few seconds. Without workers,
#' everything runs in the main process.
#' @param n Number of worker processes.
#' @return The number of workers, invisibly.
#' @export
#' @examples
#' \dontrun{
#' nfl4th_start_workers(4)
#' # ... add_4th_probs(plays, bootstrap = TRUE) ...
#' nfl4th_stop_workers()
#' }
nfl4th_start_workers <- function(
  n = max(1L, parallel::detectCores() - 1L)
) {
  nfl4th_stop_workers()
  # the draws, downloaded and checked here first
  n_draws <- length(bootstrap_models()$draws)
  f <- bootstrap_memo$file
  n <- min(as.integer(n), n_draws - 1L)
  cl <- parallel::makePSOCKcluster(n)
  chunks <- split(seq_len(n_draws)[-1], rep_len(seq_len(n), n_draws - 1L))
  parallel::clusterApply(
    cl,
    chunks,
    function(idx, f) asNamespace("nfl4th")$boot_worker_init(idx, f),
    f
  )
  nfl4th_workers$cl <- cl
  nfl4th_workers$chunks <- chunks
  invisible(n)
}

#' @rdname nfl4th_start_workers
#' @export
nfl4th_stop_workers <- function() {
  if (!is.null(nfl4th_workers$cl)) {
    try(parallel::stopCluster(nfl4th_workers$cl), silent = TRUE)
  }
  nfl4th_workers$cl <- NULL
  nfl4th_workers$chunks <- NULL
  invisible(0L)
}

# in a worker: its draws, their xgboost models loaded, one thread each (the
# workers are the parallelism)
boot_worker_init <- function(idx, f) {
  options(nfl4th.bootstrap_models = f)
  data.table::setDTthreads(1L)
  draws <- bootstrap_models()$draws
  for (b in idx) {
    booster <- wpfd_booster(draws[[b]]$wp)
    xgboost::xgb.model.parameters(booster) <- list(nthread = 1L)
  }
  # keep only this worker's draws in memory
  keep <- seq_along(draws) %in% idx
  bootstrap_memo$models$draws[!keep] <- list(NULL)
  nfl4th_workers$idx <- idx
  TRUE
}

# Steps 2 and 3 in the workers, each with its own draws; NULL without workers.
# If the workers fail, they are stopped and the play is evaluated here.
boot_eval_workers <- function(x, q) {
  cl <- nfl4th_workers$cl
  if (is.null(cl)) {
    return(NULL)
  }
  parts <- tryCatch(
    parallel::clusterCall(
      cl,
      function(x, q) {
        ns <- asNamespace("nfl4th")
        ns$boot_eval(x, q, ns$nfl4th_workers$idx)
      },
      x,
      q
    ),
    error = function(e) {
      cli::cli_warn(c(
        "The nfl4th workers failed, evaluating here instead.",
        "x" = conditionMessage(e)
      ))
      nfl4th_stop_workers()
      NULL
    }
  )
  if (is.null(parts)) {
    return(NULL)
  }
  idx <- unlist(nfl4th_workers$chunks, use.names = FALSE)
  o <- order(idx)
  pick <- function(k) do.call(cbind, lapply(parts, `[[`, k))[, o, drop = FALSE]
  lapply(stats::setNames(nm = boot_columns), pick)
}
