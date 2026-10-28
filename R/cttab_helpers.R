#' Internal helpers shared by `cttab`, `cttab_plot` and `report_missing`.
#'
#' Centralising these so the three call sites stay consistent - variable
#' label lookup, `select` filter evaluation and grouping-column factor
#' coercion all need to behave identically across the table, the plot and
#' the missing-data report.
#'
#' @name cttab_helpers
#' @keywords internal
NULL


#' Has `x` been through `cttab_format()` already?
#'
#' Centralises the "is this already a rendering-ready cttab?" check that
#' was duplicated across `cttab_format()`, `print.cttab()`,
#' `as.data.frame.cttab()`, and `write_table()`. A formatted cttab is
#' either:
#' \itemize{
#'   \item a `matrix` carrying a `row_style` attribute, or
#'   \item a `data.frame` with a `label` column (the layout produced by
#'     `cttab_format()` for new long-format inputs).
#' }
#' A plain `data.frame` carrying the `cttab` class but neither shape (the
#' user-built legacy table) is *not* formatted; callers route those
#' through the regular data.frame renderer.
#'
#' @param x A `cttab` object (or anything else; the check is permissive).
#'
#' @return `TRUE` if `x` already has a renderer-ready shape, `FALSE`
#'   otherwise.
#'
#' @keywords internal
is_formatted_cttab <- function(x) {
  if (is.matrix(x)) return(!is.null(attr(x, "row_style")))
  is.data.frame(x) && "label" %in% names(x)
}


#' Return the variable label of `x` if it has one, otherwise `fallback`.
#'
#' @param x A vector (typically a column of `data`).
#' @param fallback Value to return when `x` carries no variable label.
#'
#' @keywords internal
cttab_get_label <- function(x, fallback) {
  if (has_label(x)) var_lab(x) else fallback
}


#' Evaluate a `select` filter expression for one variable.
#'
#' `select` is a named character vector / list whose names are variable
#' names and whose values are R-expression strings evaluated against
#' `data`. The contract matches `stat_tab()`: rows where the expression is
#' `FALSE` or `NA` are excluded.
#'
#' @param data A `data.frame` / `data.table` providing the evaluation
#'   environment.
#' @param var Name of the variable whose filter should be evaluated.
#' @param select Named filter list (or `NULL`).
#'
#' @return Logical vector of length `nrow(data)`. Returns all-`TRUE` when
#'   `select` is `NULL` or when `var` has no entry in `select`. On
#'   evaluation error a warning is emitted and all-`TRUE` is returned.
#'
#' @keywords internal
cttab_eval_select <- function(data, var, select) {
  if (is.null(select) || !var %in% names(select)) {
    return(rep(TRUE, nrow(data)))
  }
  keep <- tryCatch(
    eval(parse(text = select[[var]]), envir = as.list(data), enclos = baseenv()),
    error = function(e) {
      warning("Filter failed for variable: ", var, " - ", e$message)
      rep(TRUE, nrow(data))
    }
  )
  keep[is.na(keep)] <- FALSE
  keep
}


#' Reject `select` filters that reference a grouping variable.
#'
#' The grouping / row-split variables are only valid as `group` and
#' `row_split`. Referencing one in a `select` filter is meaningless, and used
#' to produce a silently wrong table rather than an error: `.SD` excludes the
#' `by` columns, so per-group cells could not resolve the group variable and
#' went unfiltered, while the ungrouped Total chunk did filter -- leaving Total
#' equal to a single group rather than the whole population.
#'
#' @param select Named filter list (or `NULL`).
#' @param grouping_vars Character vector of grouping / row-split variable names
#'   (`NULL` entries drop out, so `c(group, row_split)` works).
#'
#' @return `NULL`, invisibly. Errors when a filter references a grouping
#'   variable.
#'
#' @keywords internal
cttab_check_select_vars <- function(select, grouping_vars) {
  if (is.null(select) || length(grouping_vars) == 0L) {
    return(invisible(NULL))
  }
  for (var in names(select)) {
    refs <- tryCatch(all.vars(parse(text = select[[var]])),
                     error = function(e) character(0))
    bad <- intersect(refs, grouping_vars)
    if (length(bad) > 0) {
      stop("The select filter for '", var, "' references the grouping or row ",
           "split variable ", paste(sQuote(bad, q = FALSE), collapse = ", "),
           ". Grouping variables can only be used as `group` or `row_split`.")
    }
  }
  invisible(NULL)
}


#' Drop rows with missing values in any grouping / row-split column.
#'
#' Returns a new `data.table` (does not mutate `data` in place). Emits a
#' `message()` listing the dropped count when any rows are removed so the
#' user is aware their group / row-split column carried `NA`s.
#'
#' @param data A `data.table`.
#' @param cols Column names to check. `NULL` entries are silently skipped
#'   so `c(group, row_split)` works when one is `NULL`.
#'
#' @return A `data.table` with the offending rows removed.
#'
#' @keywords internal
cttab_drop_na_grouping <- function(data, cols) {
  cols <- cols[nzchar(cols)]
  if (length(cols) == 0L) return(data)

  na_mask <- Reduce(`|`, lapply(cols, function(col) is.na(data[[col]])))
  n_drop <- sum(na_mask)
  if (n_drop > 0L) {
    message(sprintf(
      "Dropped %d observation%s with missing %s.",
      n_drop,
      if (n_drop == 1L) "" else "s",
      paste(sQuote(cols, q = FALSE), collapse = " / ")
    ))
    data <- data[!na_mask]
  }
  data
}


#' Coerce labelled / non-factor grouping columns to factors in place.
#'
#' Group / row-split columns produced by `apply_macro_dict()` are
#' typically labelled-numeric. ggplot will treat them as continuous (with
#' "Continuous x aesthetic" / "fill aesthetic dropped" warnings on
#' boxplots and dodged bars), and `as.character()` on the raw values
#' returns numeric codes rather than the labels we want in the
#' missingness report's `visit` column. Coercing once via
#' [to_factor()] fixes both at the source.
#'
#' Mutates `data` by reference via [data.table::set()] - pass a `copy()`
#' if the caller's `data.table` must remain untouched.
#'
#' @param data A `data.table`.
#' @param cols Character vector of column names. `NULL` entries are
#'   silently skipped, so `c(group, row_split)` works even when one is
#'   `NULL`.
#' @param drop_levels Forwarded to [to_factor()].
#'
#' @keywords internal
cttab_factorise <- function(data, cols, drop_levels = FALSE) {
  for (col in cols) {
    if (is.null(col)) next
    if (has_labels(data[[col]]) || !is.factor(data[[col]])) {
      data.table::set(
        data,
        j = col,
        value = to_factor(data[[col]], drop_levels = drop_levels)
      )
    }
  }
  invisible(data)
}



# split formulas
# From Formula package
#' @keywords internal
split_formula <- function(f) {
  stopifnot(inherits(f, "formula"))

  rhs <- if (length(f) > 2) f[[3L]] else f[[2L]]
  lhs <- if (length(f) > 2) f[[2L]] else NULL

  extract_parts <- function(x, sep = "|") {
    if (is.null(x)) {
      return(NULL)
    }

    rval <- list()
    if (length(x) > 1L && x[[1L]] == sep) {
      while (length(x) > 1L && x[[1L]] == sep) {
        rval <- c(x[[3L]], rval)
        x <- x[[2L]]
      }
    }
    return(c(x, rval))
  }

  list(lhs = extract_parts(lhs), rhs = extract_parts(rhs))
}


#' Combine descriptive statistics tables by rows.
#'
#' Stacks two or more long-format \code{\link{cttab}} objects without
#' formatting them. The inputs must share the same grouping variable
#' (\code{attr(., "group")}) so they line up as a single rendered table; the
#' \code{nest} mode is taken from the first input. The bound parts are kept
#' as separate sections in the rendered output by offsetting their
#' \code{Group_ID} / \code{Var_ID} so subsequent variables sort after earlier
#' ones rather than interleaving.
#'
#' Already-formatted matrix \code{cttab} objects (e.g. produced by
#' \code{\link{cttab_format}}) are stacked by binding their character
#' matrices and concatenating their \code{position} attributes. Mixing
#' matrix and long-format inputs is not supported.
#'
#' @param ... \code{cttab} objects to stack.
#'
#' @return A \code{cttab} object of the same flavour (long-format
#'   \code{data.table} or formatted matrix) as the inputs.
#'
#' @export
#'
rbind.cttab <- function(...) {
  allargs <- list(...)
  allargs <- Filter(Negate(is.null), allargs)

  if (length(allargs) == 0L) return(NULL)

  if (!all(vapply(allargs, inherits, logical(1L), what = "cttab"))) {
    stop("Only cttab class is supported.")
  }

  if (length(allargs) == 1L) return(allargs[[1]])

  is_mat <- vapply(allargs, is.matrix, logical(1L))

  if (all(is_mat)) {
    return(.rbind_cttab_matrix(allargs))
  }

  if (any(is_mat)) {
    stop("Cannot rbind a mix of matrix-format and long-format cttab objects;",
         " call cttab_format() on the long-format ones first if you really",
         " want to combine them.")
  }

  .rbind_cttab_long(allargs)
}

#' @keywords internal
.rbind_cttab_matrix <- function(mats) {
  rs <- unlist(lapply(mats, function(x) attr(x, "row_style")),
               use.names = FALSE)
  mats <- lapply(mats, function(x) {
    attr(x, "row_style") <- NULL
    attr(x, "class") <- NULL
    x
  })
  structure(do.call(base::rbind, mats),
    row_style = rs,
    class = c("cttab", "matrix", "array")
  )
}

#' @keywords internal
#' @import data.table
.rbind_cttab_long <- function(parts) {
  groups <- lapply(parts, attr, which = "group")
  group_unique <- unique(lapply(groups, function(g)
    if (is.null(g)) "" else g))
  if (length(group_unique) > 1L) {
    stop("Cannot rbind cttab objects with different `group` variables: ",
         paste(vapply(group_unique, function(g) if (g == "") "<none>" else g,
                      character(1L)), collapse = ", "))
  }

  # `row_split` must match for the same reason `group` must. Taking part 1's
  # attribute unchecked meant a mismatched part's split column was left out of
  # the dcast key in `.cttab_for_layout()`; the key then no longer identified
  # rows uniquely, dcast fell back to `fun.aggregate = length`, and EVERY cell
  # in the table -- including the well-formed part's -- became a row count.
  # Reversed argument order instead filed correct numbers under a fabricated
  # "<split> = NA" section. Both were silent.
  splits <- lapply(parts, attr, which = "row_split")
  split_unique <- unique(lapply(splits, function(s)
    if (is.null(s)) "" else s))
  if (length(split_unique) > 1L) {
    stop("Cannot rbind cttab objects with different `row_split` variables: ",
         paste(vapply(split_unique, function(s) if (s == "") "<none>" else s,
                      character(1L)), collapse = ", "),
         ". Stack tables that share a row split, or format them separately.")
  }

  # Offset Group_ID / Var_ID across parts so each part stays in its own
  # section once rendered. The first part keeps its IDs as-is; subsequent
  # parts are pushed past the previous part's max, including Var_ID = 0
  # (Observation) so that distinct parts each carry their own count rows.
  cum_grp <- 0L
  cum_var <- 0L
  shifted <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    dt <- as.data.table(unclass(parts[[i]]))
    if (nrow(dt) == 0L) {
      shifted[[i]] <- dt
      next
    }
    if (i > 1L) {
      # +1 so the second part's lowest id sorts strictly after the first
      # part's highest, even if both started at 0.
      dt[, Group_ID := Group_ID + cum_grp + 1L]
      dt[, Var_ID   := Var_ID   + cum_var + 1L]
    }
    cum_grp <- max(cum_grp, max(dt$Group_ID, na.rm = TRUE))
    cum_var <- max(cum_var, max(dt$Var_ID,   na.rm = TRUE))
    shifted[[i]] <- dt
  }

  out <- rbindlist(shifted, use.names = TRUE, fill = TRUE)

  # Re-build the group factor with the union of levels (Total at the end).
  grp <- groups[[1]]
  if (!is.null(grp) && grp %in% names(out)) {
    lvls <- unique(unlist(lapply(parts, function(x) {
      if (is.factor(x[[grp]])) levels(x[[grp]]) else
        as.character(unique(x[[grp]]))
    })))
    lvls <- c(setdiff(lvls, "Total"), if ("Total" %in% lvls) "Total")
    out[[grp]] <- factor(as.character(out[[grp]]), levels = lvls)
  }

  setattr(out, "group",            groups[[1]])
  setattr(out, "row_split",        attr(parts[[1]], "row_split"))
  setattr(out, "row_split_label",  attr(parts[[1]], "row_split_label"))
  setattr(out, "nest",             attr(parts[[1]], "nest") %||% "split")
  setattr(out, "class",            unique(c("cttab", class(out))))
  out[]
}

