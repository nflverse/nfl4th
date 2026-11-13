# Data frame helpers for the dplyr verbs nfl4th used, with dplyr's semantics,
# so the package does not need dplyr. Like dplyr, each keeps the class and the
# other attributes of the data frame it is given (dplyr_reconstruct()): an
# nflreadr play-by-play stays one.

# a list of columns as a data frame with `row_names` (in their stored form)
# and the class and other attributes of `template`
df_new <- function(cols, row_names, template) {
  a <- attributes(template)
  a$names <- names(cols)
  a$row.names <- row_names
  attributes(cols) <- a
  cols
}

# the columns of a data frame as a plain named list
df_cols <- function(x) {
  out <- unclass(x)
  attributes(out) <- list(names = names(x))
  out
}

# dplyr::mutate() with computed columns (a named list): each replaces the
# column of that name in place or is appended, and size 1 is recycled
df_assign <- function(x, cols) {
  n <- .row_names_info(x, 2L)
  out <- df_cols(x)
  for (nm in names(cols)) {
    out[[nm]] <- vctrs::vec_recycle(cols[[nm]], n)
  }
  df_new(out, .row_names_info(x, 0L), x)
}

# dplyr::select() by name. Names on `cols` rename
df_select <- function(x, cols) {
  new <- names(cols)
  if (is.null(new)) {
    new <- cols
  }
  new[new == ""] <- cols[new == ""]
  out <- lapply(unname(cols), function(k) .subset2(x, k))
  names(out) <- new
  df_new(out, .row_names_info(x, 0L), x)
}

# dplyr::slice() and filter(): rows by position
df_slice <- function(x, i) {
  out <- vctrs::vec_slice(x, i)
  df_new(df_cols(out), .row_names_info(out, 0L), x)
}

# dplyr::filter(): rows where `keep` is TRUE (NA drops the row)
df_filter <- function(x, keep) {
  df_slice(x, which(keep))
}

# dplyr::left_join() on equal keys (NA matches NA): every row of `x` once per
# match in `y`, in `y`'s order, or once with NA. The keys take the common type
# of both sides and the values of `x`. Other columns in both get .x and .y
# suffixes. `y` can be a list of columns
df_left_join <- function(x, y, by) {
  key <- function(d) {
    vctrs::new_data_frame(
      stats::setNames(lapply(by, function(k) d[[k]]), by),
      n = vctrs::vec_size(d[[by[1]]])
    )
  }
  xk <- key(x)
  yk <- key(y)
  ptype <- vctrs::vec_ptype_common(xk, yk)
  xk <- vctrs::vec_cast(xk, ptype)
  yk <- vctrs::vec_cast(yk, ptype)
  m <- vctrs::vec_locate_matches(
    xk,
    yk,
    incomplete = "compare",
    no_match = NA_integer_,
    multiple = "all",
    relationship = "none"
  )
  xi <- m$needles
  yi <- m$haystack
  x_names <- names(x)
  y_names <- setdiff(names(y), by)
  both <- intersect(setdiff(x_names, by), y_names)
  out_x <- lapply(x_names, function(k) {
    vctrs::vec_slice(if (k %in% by) xk[[k]] else x[[k]], xi)
  })
  names(out_x) <- if_else(x_names %in% both, paste0(x_names, ".x"), x_names)
  out_y <- lapply(y_names, function(k) vctrs::vec_slice(y[[k]], yi))
  names(out_y) <- if_else(y_names %in% both, paste0(y_names, ".y"), y_names)
  df_new(c(out_x, out_y), .set_row_names(length(xi)), x)
}

# dplyr::bind_rows() of a list of data frames: the class and attributes of the
# first
df_bind_rows <- function(dfs) {
  out <- do.call(vctrs::vec_rbind, unname(dfs))
  df_new(df_cols(out), .row_names_info(out, 0L), dfs[[1]])
}

# the row order of dplyr::arrange(): keys in a list, `desc` TRUE for a key
# sorted in descending order. Ties keep their order, NA goes last either way
# and strings sort in the C locale
df_order <- function(keys, desc = FALSE) {
  do.call(
    order,
    c(
      unname(keys),
      list(
        method = "radix",
        na.last = TRUE,
        decreasing = rep_len(desc, length(keys))
      )
    )
  )
}

# the rows of dplyr's group_by() |> slice(1): the first row of each group of
# `keys` (a list), in the order of the groups' keys
df_first_of_groups <- function(keys) {
  first <- vctrs::vec_unique_loc(vctrs::new_data_frame(
    unname(keys) |> stats::setNames(paste0("k", seq_along(keys)))
  ))
  first[df_order(lapply(keys, `[`, first))]
}

# dplyr::rename(): `cols` is c(new = "old")
df_rename <- function(x, cols) {
  i <- match(cols, names(x))
  if (anyNA(i)) {
    cli::cli_abort(
      "Can't rename columns that don't exist: {.field {cols[is.na(i)]}}."
    )
  }
  out <- df_cols(x)
  names(out)[i] <- names(cols)
  df_new(out, .row_names_info(x, 0L), x)
}
