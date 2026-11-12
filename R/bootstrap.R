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

# Every play under every draw. `x` is what add_4th_probs() passes to add_probs()
# and `point` its result with the shipped fit (draw 1).
add_boot_probs <- function(x, point) {
  bm <- bootstrap_models()
  n_draws <- length(bm$draws)
  n <- nrow(x)
  go <- fg <- punt <- matrix(NA_real_, n, n_draws)
  go[, 1] <- point$go_wp
  fg[, 1] <- point$fg_wp
  punt[, 1] <- point$punt_wp
  cli::cli_progress_bar("Bootstrap draws", total = n_draws - 1)
  for (b in seq_len(n_draws)[-1]) {
    r <- with_draw(bm$draws[[b]], add_probs(x))
    go[, b] <- r$go_wp
    fg[, b] <- r$fg_wp
    punt[, b] <- r$punt_wp
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  boost <- boot_go_boost(go, fg, punt)
  dec <- boot_decision(go, fg, punt)
  rows <- function(m) lapply(seq_len(n), function(i) m[i, ])
  qs <- apply(boost, 1, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  list(
    index = x$index,
    boot_pct = 100 * rowMeans(dec == dec[, 1]),
    go_boost_lo = qs[1, ],
    go_boost_hi = qs[2, ],
    go_boost_boot = rows(boost),
    go_wp_boot = rows(go),
    fg_wp_boot = rows(fg),
    punt_wp_boot = rows(punt)
  )
}
