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

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Check if All Elements in Character Vector are Numeric
#'
#' Tests, without issuing warnings, whether all elements of a character
#' vector are legal numeric values, or optionally converts the vector to
#'  a numeric vector. Leading and trailing blanks in x are ignored.
#'
#' @param x a character vector
#' @param extras a vector of character strings to count as numeric values,
#' other than "".
#'
#' @return a logical value
#'
#' @examples
#' all_is_numeric(c("1", "1.2", "3"))
#' all_is_numeric(c("1", "1.2", "3a"))
#'
#' @export


all_is_numeric <- function(x, extras = c(".", NA)) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  xs <- x[!x %in% c("", extras)]
  if (!length(xs)) {
    FALSE
  }
  suppressWarnings(!any(is.na(as.numeric(xs))))
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
