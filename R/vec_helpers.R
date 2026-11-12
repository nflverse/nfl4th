# Vector helpers with dplyr's semantics, so the package does not need dplyr.
# dplyr 1.2 implements if_else(), case_when() and coalesce() on vctrs'
# vec_if_else() and vec_case_when(); these make the same calls.
# nfl4th-checks/speed/11_vec_helpers_fuzz.R checks them against dplyr on
# random inputs, names and errors included.

# dplyr::if_else()
if_else <- function(condition, true, false) {
  vctrs::vec_if_else(condition, true, false)
}

# dplyr::case_when(), without its arguments: the first condition that is TRUE
# wins, NA counts as FALSE, and a row no condition takes is NA. Both sides of
# every formula are evaluated in the formula's environment.
case_when <- function(...) {
  fs <- list(...)
  conds <- lapply(fs, function(f) eval(f[[2]], environment(f)))
  vals <- lapply(fs, function(f) eval(f[[3]], environment(f)))
  size_c <- do.call(vctrs::vec_size_common, conds)
  size_v <- do.call(vctrs::vec_size_common, vals)
  n <- if (size_c == 1L) {
    size_v
  } else if (size_v == 1L || size_c == size_v) {
    size_c
  } else {
    do.call(vctrs::vec_size_common, c(conds, vals))
  }
  conds <- do.call(vctrs::vec_recycle_common, c(conds, list(.size = n)))
  vctrs::vec_case_when(conds, vals, size = n)
}

# dplyr::coalesce(): each missing value taken from the next argument that has
# one
coalesce <- function(...) {
  args <- list(...)
  ptype <- do.call(vctrs::vec_ptype_common, args)
  args <- do.call(vctrs::vec_cast_common, c(args, list(.to = ptype)))
  n <- do.call(vctrs::vec_size_common, args)
  k <- length(args)
  default <- NULL
  if (!vctrs::vec_any_missing(args[[k]])) {
    default <- args[[k]]
    args <- args[-k]
  }
  args <- do.call(vctrs::vec_recycle_common, c(args, list(.size = n)))
  conds <- lapply(args, function(v) !vctrs::vec_detect_missing(v))
  vctrs::vec_case_when(conds, args, default = default, ptype = ptype, size = n)
}
