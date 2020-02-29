# ------------------------------------------------------------------------------
# from https://raw.githubusercontent.com/tidyverse/tibble/master/R/as_tibble.R
#

#' Coerce lists, matrices, and more to data frames
#'
#' @description
#' \lifecycle{maturing}
#'
#' `as_tibble()` turns an existing object, such as a data frame or
#' matrix, into a so-called tibble, a data frame with class [`tbl_df`]. This is
#' in contrast with [tibble()], which builds a tibble from individual columns.
#' `as_tibble()` is to [`tibble()`] as [base::as.data.frame()] is to
#' [base::data.frame()].
#'
#' `as_tibble()` is an S3 generic, with methods for:
#' * [`data.frame`][base::data.frame()]: Thin wrapper around the `list` method
#'   that implements tibble's treatment of [rownames].
#' * [`matrix`][methods::matrix-class], [`poly`][stats::poly()],
#'   [`ts`][stats::ts()], [`table`][base::table()]
#' * Default: Other inputs are first coerced with [base::as.data.frame()].
#'
#' @section Row names:
#' The default behavior is to silently remove row names.
#'
#' New code should explicitly convert row names to a new column using the
#' `rownames` argument.
#'
#' For existing code that relies on the retention of row names, call
#' `pkgconfig::set_config("tibble::rownames" = NA)` in your script or in your
#' package's [.onLoad()]  function.
#'
#' @seealso [tibble()] constructs a tibble from individual columns. [enframe()]
#'   converts a named vector to a tibble with a column of names and column of
#'   values. Name repair is implemented using [vctrs::vec_as_names()].
#'
#' @param x A data frame, list, matrix, or other object that could reasonably be
#'   coerced to a tibble.
#' @param ... Other arguments passed on to individual methods.
#' @inheritParams tibble
#' @param rownames How to treat existing row names of a data frame or matrix:
#'   * `NULL`: remove row names. This is the default.
#'   * `NA`: keep row names.
#'   * A string: the name of a new column. Existing rownames are transferred
#'     into this column and the `row.names` attribute is deleted.
#'  Read more in [rownames].

#' @param _n,validate
#'   \lifecycle{soft-deprecated}
#'
#'   For compatibility only, do not use for new code.
#' @export
#' @examples
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' df <- as_tibble(m)
#'
#' # Prefer enframe() for vectors
#' enframe(1:3)
#' enframe(1:3, name = NULL)
as_tibble <- function(x, ...,
                      .rows = NULL,
                      .name_repair = c("check_unique", "unique", "universal", "minimal"),
                      rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  UseMethod("as_tibble")
}

#' @export
#' @rdname as_tibble
as_tibble.data.frame <- function(x, validate = NULL, ...,
                                 .rows = NULL,
                                 .name_repair = c("check_unique", "unique", "universal", "minimal"),
                                 rownames = pkgconfig::get_config("tibble::rownames", NULL)) {

  .name_repair <- compat_name_repair(.name_repair, validate)

  old_rownames <- raw_rownames(x)
  if (is.null(.rows)) {
    .rows <- nrow(x)
  }

  result <- lst_to_tibble(unclass(x), .rows, .name_repair)

  if (is.null(rownames)) {
    result
  } else if (is.na(rownames)) {
    attr(result, "row.names") <- old_rownames
    result
  } else {
    if (length(old_rownames) > 0 && is.na(old_rownames[1L])) {  # if implicit rownames
      old_rownames <- seq_len(abs(old_rownames[2L]))
    }
    old_rownames <- as.character(old_rownames)
    add_column(result, !!rownames := old_rownames, .before = 1L)
  }
}

#' @export
#' @rdname as_tibble
as_tibble.list <- function(x, validate = NULL, ..., .rows = NULL,
                           .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  .name_repair <- compat_name_repair(.name_repair, validate)

  lst_to_tibble(x, .rows, .name_repair, col_lengths(x))
}

lst_to_tibble <- function(x, .rows, .name_repair, lengths = NULL) {
  x <- unclass(x)
  x <- set_repaired_names(x, .name_repair)
  x <- check_valid_cols(x)
  recycle_columns(x, .rows, lengths)
}

compat_name_repair <- function(.name_repair, validate) {
  if (is.null(validate)) return(.name_repair)

  name_repair <- if (isTRUE(validate)) "check_unique" else "minimal"

  if (!has_length(.name_repair, 1)) {
    signal_soft_deprecated("The `validate` argument to `as_tibble()` is deprecated. Please use `.name_repair` to control column names.")
  } else if (.name_repair != name_repair) {
    warn("The `.name_repair` argument to `as_tibble()` takes precedence over the deprecated `validate` argument.")
    return(.name_repair)
  }

  name_repair
}

check_valid_cols <- function(x) {
  names_x <- names2(x)

  is_xd <- which(!map_lgl(x, is_valid_col))
  if (has_length(is_xd)) {
    classes <- map_chr(x[is_xd], function(x) class(x)[[1]])
    abort(error_column_must_be_vector(names_x[is_xd], attr(x, "pos")[is_xd], classes))
  }

  # 657
  x[] <- map(x, make_valid_col)
  invisible(x)
}

make_valid_col <- function(x) {
  if (is.expression(x)) {
    x <- as.list(x)
  }
  x
}

check_valid_col <- function(x, name, pos) {
  if (name == "") {
    ret <- check_valid_cols(structure(list(x), pos = pos))
  } else {
    ret <- check_valid_cols(list2(!!name := x))
  }
  invisible(ret[[1]])
}

is_valid_col <- function(x) {
  # 657
  vec_is(x) || is.expression(x)
}

recycle_columns <- function(x, .rows, lengths) {
  nrow <- guess_nrow(lengths, .rows)

  # Shortcut if all columns have the requested or implied length
  different_len <- which(lengths != nrow)
  if (is_empty(different_len)) return(new_tibble(x, nrow = nrow, subclass = NULL))

  if (any(lengths[different_len] != 1)) {
    abort(error_inconsistent_cols(.rows, names(x), lengths, "Requested with `.rows` argument"))
  }

  if (nrow != 1L) {
    short <- which(lengths == 1L)
    if (has_length(short)) {
      x[short] <- map(x[short], vec_recycle, nrow)
    }
  }

  new_tibble(x, nrow = nrow, subclass = NULL)
}

guess_nrow <- function(lengths, .rows) {
  if (!is.null(.rows)) {
    return(.rows)
  }
  if (is_empty(lengths)) {
    return(0)
  }

  nontrivial_lengths <- lengths[lengths != 1L]
  if (is_empty(nontrivial_lengths)) {
    return(1)
  }

  max(nontrivial_lengths)
}

#' @export
#' @rdname as_tibble
as_tibble.matrix <- function(x, ..., validate = NULL, .name_repair = NULL) {
  m <- matrixToDataFrame(x)
  names <- colnames(x)
  if (is.null(.name_repair)) {
    if ((is.null(names) || any(bad_names <- duplicated(names) | names == "")) && has_length(x)) {
      signal_soft_deprecated('`as_tibble.matrix()` requires a matrix with column names or a `.name_repair` argument. Using compatibility `.name_repair`.')
      compat_names <- paste0("V", seq_along(m))
      if (is.null(names)) {
        names <- compat_names
      } else {
        names[bad_names] <- compat_names[bad_names]
      }
      .name_repair <- function(x) names
    } else {
      .name_repair <- "check_unique"
    }
    validate <- NULL
  }
  colnames(m) <- names
  as_tibble(m, ..., validate = validate, .name_repair = .name_repair)
}

#' @export
as_tibble.poly <- function(x, ...) {
  m <- matrixToDataFrame(unclass(x))
  colnames(m) <- colnames(x)
  as_tibble(m, ...)
}

#' @export
as_tibble.ts <- function(x, ..., .name_repair = "minimal") {
  df <- as.data.frame(x)
  if (length(dim(x)) == 2) {
    colnames(df) <- colnames(x)
  }
  as_tibble(df, ..., .name_repair = .name_repair)
}

#' @export
#' @param n Name for count column, default: `"n"`.
#' @rdname as_tibble
as_tibble.table <- function(x, `_n` = "n", ..., n = `_n`) {
  if (!missing(`_n`)) {
    warn("Please pass `n` as a named argument to `as_tibble.table()`.")
  }

  df <- as.data.frame(x, stringsAsFactors = FALSE)
  names(df) <- c(names2(dimnames(x)), n)

  as_tibble(df, ...)
}

#' @export
#' @rdname as_tibble
as_tibble.NULL <- function(x, ...) {
  if (missing(x)) {
    signal_soft_deprecated(error_as_tibble_needs_argument())
  }

  new_tibble(list(), nrow = 0)
}

#' @export
#' @rdname as_tibble
as_tibble.default <- function(x, ...) {
  value <- x
  if (is_atomic(value)) {
    signal_soft_deprecated("Calling `as_tibble()` on a vector is discouraged, because the behavior is likely to change in the future. Use `tibble::enframe(name = NULL)` instead.")
  }
  as_tibble(as.data.frame(value, stringsAsFactors = FALSE), ...)
}


# ------------------------------------------------------------------------------
# Original content
# from https://raw.githubusercontent.com/tidyverse/tidyr/master/R/expand.R
#

#' Expand data frame to include all possible combinations of values
#'
#' @description
#' `expand()` generates all combination of variables found in a dataset.
#' It is paired with `nesting()` and `crossing()` helpers. `crossing()`
#' is a wrapper around [expand_grid()] that de-duplicates and sorts its inputs;
#' `nesting()` is a helper that only finds combinations already present in the
#' data.
#'
#' `expand()` is often useful in conjunction with joins:
#'
#'  * use it with `right_join()` to convert implicit missing values to
#'    explicit missing values (e.g., fill in gaps in your data frame).
#'  * use it with `anti_join()` to figure out which combinations are missing
#'    (e.g., identify gaps in your data frame).
#'
#' @inheritParams expand_grid
#' @param data A data frame.
#' @param ... Specification of columns to expand. Columns can be atomic vectors
#'   or lists.
#'
#'   * To find all unique combinations of `x`, `y` and `z`, including those not
#'     present in the data, supply each variable as a separate argument:
#'     `expand(df, x, y, z)`.
#'   * To find only the combinations that occur in the
#'     data, use `nesting`: `expand(df, nesting(x, y, z))`.
#'   * You can combine the two forms. For example,
#'     `expand(df, nesting(school_id, student_id), date)` would produce
#'     a row for each present school-student combination for all possible
#'     dates.
#'
#'   When used with factors, `expand()` uses the full set of levels, not just
#'   those that appear in the data. If you want to use only the values seen in
#'   the data, use `forcats::fct_drop()`.
#'
#'   When used with continuous variables, you may need to fill in values
#'   that do not appear in the data: to do so use expressions like

#'   `year = 2010:2020` or `year = \link{full_seq}(year,1)`.
#' @seealso [complete()] to expand list objects. [expand_grid()]
#'   to input vectors rather than a data frame.
#' @export
#' @examples
#' fruits <- tibble(
#'   type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
#'   year   = c(2010, 2010, 2012, 2010, 2010, 2012),
#'   size  =  factor(
#'     c("XS", "S",  "M", "S", "S", "M"),
#'     levels = c("XS", "S", "M", "L")
#'   ),
#'   weights = rnorm(6, as.numeric(size) + 2)
#' )
#'
#' # All possible combinations ---------------------------------------
#' # Note that all defined, but not necessarily present, levels of the
#' # factor variable `size` are retained.
#' fruits %>% expand(type)
#' fruits %>% expand(type, size)
#' fruits %>% expand(type, size, year)
#'
#' # Only combinations that already appear in the data ---------------
#' fruits %>% expand(nesting(type))
#' fruits %>% expand(nesting(type, size))
#' fruits %>% expand(nesting(type, size, year))
#'
#' # Other uses -------------------------------------------------------
#' # Use with `full_seq()` to fill in values of continuous variables
#' fruits %>% expand(type, size, full_seq(year, 1))
#' fruits %>% expand(type, size, 2010:2012)
#'
#' # Use `anti_join()` to determine which observations are missing
#' all <- fruits %>% expand(type, size, year)
#' all
#' all %>% dplyr::anti_join(fruits)
#'
#' # Use with `right_join()` to fill in missing rows
#' fruits %>% dplyr::right_join(all)
expand <- function(data, ..., .name_repair = "check_unique") {
  UseMethod("expand")
}

#' @export
expand.data.frame <- function(data, ..., .name_repair = "check_unique") {
  cols <- dots_cols(..., `_data` = data)
  cols[] <- map(cols, sorted_unique)

  out <- expand_grid(!!!cols, .name_repair = .name_repair)
  out <- flatten_nested(out, attr(cols, "named"), .name_repair = .name_repair)

  reconstruct_tibble(data, out)
}

#' @export
expand.grouped_df <- function(data, ..., .name_repair = "check_unique") {
  dplyr::do(data, expand(., ..., .name_repair = .name_repair))
}

# Nesting & crossing ------------------------------------------------------

#' @rdname expand
#' @export
crossing <- function(..., .name_repair = "check_unique") {
  cols <- dots_cols(...)
  cols[] <- map(cols, sorted_unique)

  out <- expand_grid(!!!cols, .name_repair = .name_repair)
  flatten_nested(out, attr(cols, "named"), .name_repair)
}

sorted_unique <- function(x) {
  if (is.factor(x)) {
    # forcats::fct_unique
    factor(levels(x), levels(x), exclude = NULL, ordered = is.ordered(x))
  } else if (is_bare_list(x)) {
    vec_unique(x)
  } else {
    vec_sort(vec_unique(x))
  }
}

#' @rdname expand
#' @export
nesting <- function(..., .name_repair = "check_unique") {
  cols <- dots_cols(...)
  out <- sorted_unique(tibble(!!!cols, .name_repair = .name_repair))
  flatten_nested(out, attr(cols, "named"), .name_repair)
}

# expand_grid -------------------------------------------------------------

#' Create a tibble from all combinations of inputs
#'
#' @section Compared to [expand.grid]:
#' * Varies the first element fastest.
#' * Never converts strings to factors.
#' * Does not add any additional attributes.
#' * Returns a tibble, not a data frame.
#' * Can expand any generalised vector, including data frames.
#' @param ... Name-value pairs. The name will become the column name in the
#'   output.
#' @inheritParams as_tibble
#' @return A tibble with one column for each input in `...`. The output
#'   will have one row for each combination of the inputs, i.e. the size
#'   be equal to the product of the sizes of the inputs. This implies
#'   that if any input has length 0, the output will have zero rows.
#' @export
#' @examples
#' expand_grid(x = 1:3, y = 1:2)
#' expand_grid(l1 = letters, l2 = LETTERS)
#'
#' # Can also expand data frames
#' expand_grid(df = data.frame(x = 1:2, y = c(2, 1)), z = 1:3)
#' # And matrices
#' expand_grid(x1 = matrix(1:4, nrow = 2), x2 = matrix(5:8, nrow = 2))
expand_grid <- function(..., .name_repair = "check_unique") {
  dots <- dots_cols(...)

  # Generate sequence of indices
  ns <- map_int(dots, vec_size)
  n <- prod(ns)

  if (n == 0) {
    out <- map(dots, vec_slice, integer())
  } else {
    each <- n / cumprod(ns)
    times <- n / each / ns

    out <- pmap(list(x = dots, each = each, times = times), vec_repeat)
  }
  out <- as_tibble(out, .name_repair = .name_repair)

  flatten_nested(out, attr(dots, "named"), .name_repair)
}

# Helpers -----------------------------------------------------------------

dots_cols <- function(..., `_data` = NULL) {
  dots <- enquos(...)
  named <- names(dots) != ""

  dots <- quos_auto_name(dots)

  out <- as.list(`_data`)
  for (i in seq_along(dots)) {
    out[[names(dots)[[i]]]] <- eval_tidy(dots[[i]], data = out)
  }
  out <- out[names(dots)]

  is_null <- map_lgl(out, is.null)
  if (any(is_null)) {
    out <- out[!is_null]
    named <- named[!is_null]
  }

  structure(out, named = named)
}

# flatten unnamed nested data frames to preserve existing behaviour
flatten_nested <- function(x, named, .name_repair) {
  to_flatten <- !named & unname(map_lgl(x, is.data.frame))
  out <- flatten_at(x, to_flatten)
  as_tibble(out, .name_repair = .name_repair)
}

flatten_at <- function(x, to_flatten) {
  if (!any(to_flatten)) {
    return(x)
  }

  cols <- rep(1L, length(x))
  cols[to_flatten] <- map_int(x[to_flatten], length)

  out <- vector("list", sum(cols))
  names <- vector("character", sum(cols))
  j <- 1
  for (i in seq_along(x)) {
    if (cols[[i]] == 0) {
      next
    }

    if (to_flatten[[i]]) {
      out[j:(j + cols[[i]] - 1)] <- x[[i]]
      names[j:(j + cols[[i]] - 1)] <- names(x[[i]])
    } else {
      out[[j]] <- x[[i]]
      names[[j]] <- names(x)[[i]]
    }
    j <- j + cols[[i]]
  }
  names(out) <- names
  out
}

