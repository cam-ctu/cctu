#' Convert a long-format cttab to a rendering-ready matrix.
#'
#' Reshapes the long-format object from \code{\link{cttab}} into a wide
#' character matrix and stamps an \code{row_style} attribute (one value per
#' row) for \code{print.cttab} / \code{\link{write_table}} to consume.
#'
#' Section banners (the \code{row_split} value, the named-list label) and
#' variable-label headers are inserted automatically. The four
#' \code{row_style} values are \code{"banner"}, \code{"header"},
#' \code{"bold"} (single bold data row, e.g. logical / Observation) and
#' \code{""} (ordinary stat row). Stat-row indentation is carried as leading
#' whitespace on the row name; \code{\link{write_table}} translates it to a
#' Word indent. Empty-stat-row drop is performed upstream by
#' \code{\link{cttab}}, so this function only does layout.
#'
#' The \code{nest} attribute on \code{x} controls the row hierarchy:
#' \code{"split"} = row-split outer / variable inner; \code{"var"} flips it.
#'
#' @param x A \code{cttab} object returned by \code{\link{cttab}}.
#' @param indent Visual indent string for stat rows. Default two spaces.
#' @return A character \code{matrix} with a \code{row_style} attribute and
#'   class \code{c("cttab", "matrix")}.
#' @seealso \code{\link{cttab}}, \code{\link{write_table}}
#' @export
cttab_format <- function(x, indent = "  ") {
  if (is.matrix(x) && !is.null(attr(x, "row_style"))) {
    return(x)
  }
  if (is.null(x) || !inherits(x, "data.frame") || nrow(x) == 0L) {
    out <- matrix(character(0), nrow = 0L, ncol = 1L,
                  dimnames = list(NULL, "Total"))
    structure(out, row_style = character(0),
              class = c("cttab", "matrix", "array"))
  } else {
    .cttab_format_long(x, indent = indent)
  }
}

# Internal worker -----------------------------------------------------------
#' @keywords internal
#' @import data.table
.cttab_format_long <- function(x, indent = "  ") {
  group     <- attr(x, "group")
  row_split <- attr(x, "row_split")
  nest      <- attr(x, "nest")
  if (is.null(nest)) nest <- "split"

  dt <- as.data.table(unclass(x))

  meta_cols <- c(
    if (!is.null(row_split)) row_split,
    "Group_ID", "Group_Label", "Var_ID", "Variable",
    "Stat_ID", "Statistic", "Is_Missing", "Row_Style"
  )

  if (!is.null(group)) {
    f <- as.formula(paste(paste(meta_cols, collapse = " + "), "~", group))
    wide <- dcast(dt, f, value.var = "Value", fill = "")
    data_cols <- setdiff(names(wide), meta_cols)
    grp_levels <- levels(dt[[group]])
    if (!is.null(grp_levels)) {
      data_cols <- intersect(grp_levels, data_cols)
      setcolorder(wide, c(meta_cols, data_cols))
    }
  } else {
    wide <- copy(dt)
    setnames(wide, "Value", "Total")
    data_cols <- "Total"
  }

  rs_lab <- if (!is.null(row_split)) {
    attr(x, "row_split_label") %||% row_split
  } else {
    NULL
  }

  st <- new.env(parent = emptyenv())
  st$rows  <- list()
  st$style <- character(0)
  st$names <- character(0)

  push_row <- function(name, values, style) {
    n <- length(st$style) + 1L
    st$rows[[n]]  <- as.character(values)
    st$names[n]   <- name
    st$style[n]   <- as.character(style)
  }
  pop_row <- function() {
    n <- length(st$style)
    if (n > 0L) {
      st$rows[[n]] <- NULL
      st$style    <- st$style[-n]
      st$names    <- st$names[-n]
    }
  }
  n_rows <- function() length(st$style)

  if (is.null(row_split) || nest == "split") {
    .render_split_outer(wide, row_split, rs_lab, data_cols, indent,
                        push_row, pop_row, n_rows)
  } else {
    .render_var_outer(wide, row_split, rs_lab, data_cols, indent,
                      push_row, pop_row, n_rows)
  }

  if (length(st$rows) == 0L) {
    mat <- matrix(character(0), nrow = 0L, ncol = length(data_cols),
                  dimnames = list(NULL, data_cols))
    return(structure(mat, row_style = character(0),
                     class = c("cttab", "matrix", "array")))
  }

  # "banner" -> "header" downgrade when immediately followed by "bold".
  styles <- st$style
  next_style <- c(styles[-1], "")
  upgrade <- which(styles == "banner" & next_style == "bold")
  if (length(upgrade)) styles[upgrade] <- "header"

  mat <- do.call(rbind, st$rows)
  rownames(mat) <- st$names
  colnames(mat) <- data_cols

  structure(mat,
            row_style = styles,
            class = c("cttab", "matrix", "array"))
}

# Emit a variable block (header + stats, or a single-row variable). Stat rows
# get their visible indent prepended to the row name. Returns the number of
# rows pushed. With \code{emit_header = FALSE} the caller has already pushed
# a header for this block (used by the var-outer layout where the variable
# label sits above the per-row_split sub-sections).
#'
#' Drop-empty filtering is handled upstream by \code{stat_tab}, so every row
#' reaching this function is meant to be rendered.
#' @keywords internal
.emit_var_block <- function(sub_var, data_cols, indent, push_row,
                            header_style = "header",
                            emit_header = TRUE) {
  vid     <- sub_var$Var_ID[1]
  var_lbl <- sub_var$Variable[1]
  val_mat <- as.matrix(sub_var[, data_cols, with = FALSE])
  empty_vals <- rep("", length(data_cols))

  if (vid == 0L) {
    for (rr in seq_len(nrow(sub_var))) {
      push_row(as.character(sub_var$Variable[rr]), val_mat[rr, ], "bold")
    }
    return(nrow(sub_var))
  }

  if (any(sub_var$Row_Style == "bold")) {
    for (rr in seq_len(nrow(sub_var))) {
      push_row(var_lbl, val_mat[rr, ], "bold")
    }
    return(nrow(sub_var))
  }

  n <- 0L
  if (emit_header) {
    push_row(var_lbl, empty_vals, header_style)
    n <- n + 1L
  }
  for (rr in seq_len(nrow(sub_var))) {
    push_row(paste0(indent, as.character(sub_var$Statistic[rr])),
             val_mat[rr, ], "")
    n <- n + 1L
  }
  n
}

# Layout: row_split = outer, variable = inner.
# Levels of the row_split column in display order, or list(NULL) when there
# is no row_split. Used by both render functions.
#' @keywords internal
.rs_levels <- function(wide, row_split) {
  if (is.null(row_split)) return(list(NULL))
  lv <- levels(wide[[row_split]])
  if (is.null(lv) || !length(lv)) as.list(unique(wide[[row_split]])) else
    as.list(lv)
}

# Emit the Group_Label banner once per (outer-section, Group_ID), only when
# the label is non-NA. Returns a list with the updated seen_group_ids and
# the number of rows pushed (0 or 1).
#' @keywords internal
.maybe_emit_group_label <- function(sub_var, seen_group_ids, empty_vals,
                                    push_row) {
  gid <- sub_var$Group_ID[1]
  if (gid <= 0L || gid %in% seen_group_ids) {
    return(list(seen = seen_group_ids, pushed = 0L))
  }
  gl <- sub_var$Group_Label[1]
  pushed <- 0L
  if (!is.na(gl) && nzchar(gl)) {
    push_row(gl, empty_vals, "banner")
    pushed <- 1L
  }
  list(seen = c(seen_group_ids, gid), pushed = pushed)
}

#' @keywords internal
.render_split_outer <- function(wide, row_split, rs_lab, data_cols, indent,
                                push_row, pop_row, n_rows) {
  ord_cols <- c(if (!is.null(row_split)) row_split,
                "Group_ID", "Var_ID", "Is_Missing", "Stat_ID")
  setorderv(wide, ord_cols)

  empty_vals <- rep("", length(data_cols))

  for (rs_key in .rs_levels(wide, row_split)) {
    if (is.null(row_split)) {
      sub_rs <- wide
    } else {
      sub_rs <- wide[wide[[row_split]] == rs_key, ]
      if (nrow(sub_rs) == 0L) next
      push_row(paste(rs_lab, "=", as.character(rs_key)), empty_vals, "banner")
    }

    seen_group_ids <- integer(0)
    var_keys <- unique(sub_rs[, .(Group_ID, Var_ID)])
    rendered <- 0L

    for (k in seq_len(nrow(var_keys))) {
      gid <- var_keys$Group_ID[k]
      vid <- var_keys$Var_ID[k]
      sub_var <- sub_rs[Group_ID == gid & Var_ID == vid, ]
      if (nrow(sub_var) == 0L) next

      gl_res <- .maybe_emit_group_label(sub_var, seen_group_ids,
                                        empty_vals, push_row)
      seen_group_ids <- gl_res$seen
      rendered <- rendered + gl_res$pushed +
        .emit_var_block(sub_var, data_cols, indent, push_row)
    }

    if (!is.null(row_split) && rendered == 0L) pop_row()
  }
}

# Layout: variable = outer, row_split = inner sub-sections.
# variable label = "banner", row_split sub-header = "header",
# stats = "" (single-row vars stay "bold" directly under the variable).
#' @keywords internal
.render_var_outer <- function(wide, row_split, rs_lab, data_cols, indent,
                              push_row, pop_row, n_rows) {
  ord_cols <- c("Group_ID", "Var_ID",
                if (!is.null(row_split)) row_split,
                "Is_Missing", "Stat_ID")
  setorderv(wide, ord_cols)

  empty_vals <- rep("", length(data_cols))

  var_keys  <- unique(wide[, .(Group_ID, Var_ID)])
  rs_levels <- unlist(.rs_levels(wide, row_split))

  seen_group_ids <- integer(0)

  for (k in seq_len(nrow(var_keys))) {
    gid <- var_keys$Group_ID[k]
    vid <- var_keys$Var_ID[k]
    sub_var_all <- wide[Group_ID == gid & Var_ID == vid, ]
    if (nrow(sub_var_all) == 0L) next

    seen_group_ids <- .maybe_emit_group_label(sub_var_all, seen_group_ids,
                                              empty_vals, push_row)$seen

    var_lbl <- sub_var_all$Variable[1]

    if (vid == 0L) {
      push_row("Observation", empty_vals, "banner")
      n_pushed <- 1L
      for (rs in rs_levels) {
        sub_rs <- sub_var_all[get(row_split) == rs, ]
        if (nrow(sub_rs) == 0L) next
        val_mat <- as.matrix(sub_rs[, data_cols, with = FALSE])
        push_row(paste(rs_lab, "=", as.character(rs)), val_mat[1, ], "bold")
        n_pushed <- n_pushed + 1L
      }
      if (n_pushed == 1L) pop_row()  # drop empty Observation header
      next
    }

    # Single-row variables (logicals) collapse into per-row_split bold rows
    # under the variable banner; no need for a separate sub-section header.
    is_single_row_var <- any(sub_var_all$Row_Style == "bold")

    push_row(var_lbl, empty_vals, "banner")
    rendered_inside <- 0L

    for (rs in rs_levels) {
      sub_rs <- sub_var_all[get(row_split) == rs, ]
      if (nrow(sub_rs) == 0L) next

      if (is_single_row_var) {
        val_mat <- as.matrix(sub_rs[, data_cols, with = FALSE])
        push_row(paste(rs_lab, "=", as.character(rs)),
                 val_mat[1, ], "bold")
        rendered_inside <- rendered_inside + 1L
        next
      }

      push_row(paste(rs_lab, "=", as.character(rs)), empty_vals, "header")
      r <- .emit_var_block(sub_rs, data_cols, indent, push_row,
                           emit_header = FALSE)
      if (r == 0L) {
        pop_row()  # no usable stats -> drop sub-section header
      } else {
        rendered_inside <- rendered_inside + 1L + r
      }
    }

    if (rendered_inside == 0L) {
      pop_row()  # variable section ended up empty -> drop variable header
    }
  }
}
