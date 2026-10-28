#' Convert a long-format cttab to a rendering-ready table.
#'
#' Reshapes the long-format object from \code{\link{cttab}} into a wide
#' character data.frame and stamps a \code{row_style} attribute (one
#' string per row) for \code{print.cttab} / \code{\link{write_table}} to
#' consume. A \code{label} column holds the display label for each row
#' (formerly the matrix row-names).
#'
#' Section banners (the \code{row_split} value, the named-list label) and
#' variable-label headers are inserted automatically. \code{row_style}
#' is a per-row character vector whose entries are \code{;}-joined
#' tokens drawn from \code{\{"bold", "bgcol", "span", "indent"\}}.
#' \code{""} marks a plain stat row. Common combinations:
#' \tabular{ll}{
#'   \code{"bold;bgcol;span"} \tab section banner (grey background, spanned). \cr
#'   \code{"bold;span"} \tab variable-label / sub-section header. \cr
#'   \code{"bold"} \tab single bold data row (Observation / logical). \cr
#'   \code{"indent"} \tab indented stat row. \cr
#'   \code{""} \tab plain stat row. \cr
#' }
#' Empty-stat-row drop is performed upstream by \code{\link{cttab}}, so
#' this function only does layout.
#'
#' The \code{nest} attribute on \code{x} controls the row hierarchy:
#' \code{"split"} = row-split outer / variable inner; \code{"var"} flips it.
#' The actual layout is delegated to \code{\link{group_data}}.
#'
#' @param x A \code{cttab} object returned by \code{\link{cttab}}.
#' @return A \code{data.frame} with a \code{label} column, one column per
#'   group level (or \code{Total}), a \code{row_style} attribute, and class
#'   \code{c("cttab", "data.frame")}.
#' @seealso \code{\link{cttab}}, \code{\link{write_table}},
#'   \code{\link{group_data}}
#' @export
cttab_format <- function(x) {
  # Already-formatted matrix or data.frame-with-label - pass through.
  if (is_formatted_cttab(x)) return(x)
  if (is.null(x) || !inherits(x, "data.frame") || nrow(x) == 0L) {
    return(.cttab_empty(character(0)))
  }
  # Plain data.frame stamped with cttab class - let write_table render it.
  if (!"Value" %in% names(x)) return(x)
  .cttab_format_long(x)
}

# Internal worker -----------------------------------------------------------
# Reshape the long-format cttab into a renderer-ready data.frame.
#
# Variable / Group_Label / row_split values are kept consistent across
# nest modes (e.g. row_split values are prefixed with "rs_lab = " once
# upfront) so the same input renders to the same set of labels in
# nest = "split" and nest = "var".
#' @keywords internal
#' @import data.table
.cttab_format_long <- function(x) {
  .vkey <- NULL  # data.table NSE helper column (see globalvariables.R note)
  wide      <- .cttab_for_layout(x)
  row_split <- attr(wide, "row_split")
  rs_lab    <- attr(wide, "row_split_label")
  nest      <- attr(wide, "nest")
  data_cols <- attr(wide, "data_cols")

  if (nrow(wide) == 0L) return(.cttab_empty(data_cols))

  in_var_mode <- nest == "var" && !is.null(row_split)

  if (!is.null(row_split)) {
    set(wide, j = row_split,
        value = paste(rs_lab, "=", as.character(wide[[row_split]])))
  }

  # ---- Identify "bold-merged" rows ----------------------------------------
  wide[, Row_Style := ""]
  wide[Var_ID == 0L, Row_Style := "bold"]
  bm_by <- c(if (!is.null(row_split)) row_split, "Var_ID")
  wide[Var_ID > 0L,
       Row_Style := fifelse(.N == 1L & is_empty(Statistic), "bold", Row_Style),
       by = bm_by]

  is_bold_merged <- wide$Row_Style == "bold"
  if (in_var_mode) {
    wide[is_bold_merged, Statistic := as.character(get(row_split))]
  } else {
    wide[is_bold_merged, Statistic := Variable]
  }

  # ---- Inner group_data ---------------------------------------------------
  if (!in_var_mode) wide[, .vkey := Var_ID]
  inner_var  <- if (in_var_mode) row_split else ".vkey"
  carry_cols <- c("Group_ID", "Group_Label", "Var_ID", "Row_Style",
                  if (!in_var_mode) "Variable")

  .make_table_format <- function(d) {
    if (nrow(d) == 0L) return(d)
    res <- group_data(d, groups = inner_var,
                      carry = carry_cols,
                      shift_to = "Statistic")
    res <- res[!(is.na(Stat_ID) & Row_Style %in% "bold"), ]
    if (!in_var_mode) {
      hdr <- is.na(res$Stat_ID)
      if (any(hdr)) {
        set(res, i = which(hdr), j = "Statistic",
            value = as.character(res$Variable[hdr]))
      }
    }
    res
  }

  if (is.null(row_split)) {
    w <- .make_table_format(wide)
  } else {
    if (nest == "split") {
      lvls <- as.character(unique(wide[[row_split]]))
      parts <- lapply(lvls, function(lv)
        .make_table_format(wide[wide[[row_split]] == lv, ]))
      names(parts) <- lvls   # already in "rs_lab = X" form
    } else {
      lvls <- as.character(unique(wide$Variable))
      parts <- lapply(lvls, function(lv)
        .make_table_format(wide[Variable == lv, ]))
      names(parts) <- lvls
    }
    w <- rbindlist(parts, idcol = "split", use.names = TRUE, fill = TRUE)
    w <- group_data(w, groups = "split", shift_to = "Statistic")
  }

  # ---- Group_Label banners ------------------------------------------------
  if ("Group_Label" %in% names(w) &&
      any(!is.na(w$Group_Label) & nzchar(as.character(w$Group_Label)))) {
    w <- .insert_grplab_banners(w)
  }

  # ---- Style indices ------------------------------------------------------
  outer_banner  <- which(is.na(w$Stat_ID) & is.na(w$Var_ID) & is.na(w$Group_ID))
  grplab_banner <- which(is.na(w$Stat_ID) & is.na(w$Var_ID) & !is.na(w$Group_ID))
  inner_header  <- which(is.na(w$Stat_ID) & !is.na(w$Var_ID))
  bold_data     <- which(!is.na(w$Stat_ID) & w$Row_Style %in% "bold")
  stat_data     <- which(!is.na(w$Stat_ID) & !(w$Row_Style %in% "bold"))

  grplab_collapsed <- integer(0)
  if (length(grplab_banner)) {
    nxt_is_bold <- (grplab_banner + 1L) %in% bold_data
    grplab_collapsed <- grplab_banner[nxt_is_bold]
  }
  grplab_banner_bgcol <- setdiff(grplab_banner, grplab_collapsed)

  idx_bold   <- sort(c(outer_banner, grplab_banner, inner_header, bold_data))
  idx_bgcol  <- sort(c(outer_banner, grplab_banner_bgcol))
  idx_span   <- sort(c(outer_banner, grplab_banner, inner_header))
  idx_indent <- stat_data

  for (col in data_cols) {
    v <- as.character(w[[col]])
    v[is.na(v)] <- ""
    set(w, j = col, value = v)
  }

  out <- data.frame(label = as.character(w$Statistic),
                    stringsAsFactors = FALSE)
  for (col in data_cols) out[[col]] <- as.character(w[[col]])

  out <- format_table(out,
                      bold   = idx_bold,
                      bgcol  = idx_bgcol,
                      span   = idx_span,
                      indent = idx_indent)
  class(out) <- c("cttab", "data.frame")
  out
}

# Insert "Group_Label" banner rows at the top of each Group_ID chunk
# whose Group_Label is non-NA / non-empty. Banner rows carry Group_ID so
# the `grplab_banner` style bucket picks them up (Stat_ID / Var_ID stay
# NA so they don't get classified as inner headers).
#' @keywords internal
#' @import data.table
.insert_grplab_banners <- function(w) {
  # data.table NSE helper columns (see globalvariables.R note).
  `._idx` <- `._section` <- NULL # nolint: object_name_linter.
  w <- copy(w)
  w[, ._idx := as.numeric(.I)]
  is_outer <- is.na(w$Stat_ID) & is.na(w$Var_ID) & is.na(w$Group_ID)
  w[, ._section := cumsum(is_outer)]
  first_occ <- w[!is.na(Group_Label) & nzchar(as.character(Group_Label)),
                 .(insert_at = min(._idx),
                   label     = as.character(Group_Label[1L])),
                 by = c("._section", "Group_ID")]
  w[, ._section := NULL]
  if (nrow(first_occ) == 0L) {
    w[, ._idx := NULL]
    return(w)
  }
  banners <- data.table(
    Statistic   = first_occ$label,
    Group_ID    = first_occ$Group_ID,
    Group_Label = NA_character_,
    Var_ID      = NA_integer_,
    Stat_ID     = NA_integer_,
    Row_Style   = NA_character_,
    ._idx       = first_occ$insert_at - 0.5
  )
  combined <- rbindlist(list(w, banners), use.names = TRUE, fill = TRUE)
  setorder(combined, ._idx)
  combined[, ._idx := NULL]
  combined
}



# Empty-table sentinel, used in three spots in the format pipeline.
#' @keywords internal
.cttab_empty <- function(data_cols) {
  out <- data.frame(label = character(0), stringsAsFactors = FALSE)
  for (col in data_cols) out[[col]] <- character(0)
  structure(out, row_style = character(0),
            class = c("cttab", "data.frame"))
}

# Internal pre-processor: pivot long-format cttab to wide, sort, attach attrs.
#' @keywords internal
#' @import data.table
.cttab_for_layout <- function(x) {
  group     <- attr(x, "group")
  row_split <- attr(x, "row_split")
  nest      <- attr(x, "nest") %||% "split"

  dt <- as.data.table(unclass(x))

  # NULL row_split drops out of c() automatically.
  meta_cols <- c(row_split,
                 "Group_ID", "Group_Label", "Var_ID", "Variable",
                 "Stat_ID", "Statistic")

  if (!is.null(group)) {
    f <- as.formula(paste(paste(meta_cols, collapse = " + "), "~", group))
    # Guard the cast key. If `meta_cols + group` does not identify rows
    # uniquely, dcast silently defaults to `fun.aggregate = length` and every
    # value in the table -- not just the offending rows -- is replaced by a row
    # COUNT. The numbers stay small and plausible, so it does not look broken
    # in a Word report. Refuse instead of rendering counts as statistics.
    if (anyDuplicated(dt, by = c(meta_cols, group)) > 0L) {
      stop("Cannot render this cttab: the rows are not uniquely identified by ",
           paste(c(meta_cols, group), collapse = " + "),
           ". This usually means cttab objects with different `row_split` ",
           "variables were combined.")
    }
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

  rs_lab <- if (is.null(row_split)) NULL else
    attr(x, "row_split_label") %||% row_split


  setorderv(wide, c(row_split, "Group_ID", "Var_ID", "Stat_ID"))

  attrs <- list(group = group, row_split = row_split,
                row_split_label = rs_lab, nest = nest,
                data_cols = data_cols)
  for (nm in names(attrs)) setattr(wide, nm, attrs[[nm]])
  wide
}

