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
  # Already-formatted matrix or data.frame-with-label — pass through.
  if (is_formatted_cttab(x)) return(x)
  if (is.null(x) || !inherits(x, "data.frame") || nrow(x) == 0L) {
    return(.cttab_empty(character(0)))
  }
  # Plain data.frame stamped with cttab class — let write_table render it.
  if (!"Value" %in% names(x)) return(x)
  .cttab_format_long(x)
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
                 "Stat_ID", "Statistic", "Is_Missing", "Row_Style")

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

  rs_lab <- if (is.null(row_split)) NULL else
    attr(x, "row_split_label") %||% row_split

  setorderv(wide, c(row_split, "Group_ID", "Var_ID", "Is_Missing", "Stat_ID"))

  attrs <- list(group = group, row_split = row_split,
                row_split_label = rs_lab, nest = nest,
                data_cols = data_cols)
  for (nm in names(attrs)) setattr(wide, nm, attrs[[nm]])
  wide
}

# Internal worker -----------------------------------------------------------
# Lays out the wide table by delegating to group_data(). The hierarchy is:
#
#   nest = "split" : row_split  > Group_Label > Variable  > stats
#   nest = "var"   : Group_Label > Variable   > row_split > stats
#
# Single-row "bold" variables (logicals, Observation rows with Var_ID == 0)
# carry the variable label on the data row itself, so the corresponding
# header level is suppressed via skip_header_fn (Variable in split mode,
# inner row_split in var mode). Without row_split, both nests render
# identically (split-style).
#
# Variable / Group_Label are factor-ised in Var_ID / Group_ID order before
# the call so group_data's setorderv preserves the input ordering rather
# than alphabetising — and so a Var_ID-keyed grouping survives label
# collisions (rbind.cttab post-shift "Observation" rows in particular).
#' @keywords internal
#' @import data.table
.cttab_format_long <- function(x) {
  wide      <- .cttab_for_layout(x)
  row_split <- attr(wide, "row_split")
  rs_lab    <- attr(wide, "row_split_label")
  nest      <- attr(wide, "nest")
  data_cols <- attr(wide, "data_cols")

  if (nrow(wide) == 0L) return(.cttab_empty(data_cols))

  w <- wide

  # Var_ID-keyed factor for the variable level, with a label-lookup table
  # for the rendered header text.
  vid_lab <- unique(w[, .(Var_ID, Variable)])
  setorderv(vid_lab, "Var_ID")
  vid_lvls   <- as.character(vid_lab$Var_ID)
  vid_to_lbl <- setNames(as.character(vid_lab$Variable), vid_lvls)
  w[, .vkey := factor(as.character(Var_ID), levels = vid_lvls)]

  # Group_Label as factor in Group_ID order (with empty strings normalised
  # to NA so the banner is suppressed by skip_na_label, matching the prior
  # nzchar() guard in .maybe_emit_group_label).
  has_grplab <- "Group_Label" %in% names(w)
  if (has_grplab) {
    w[!is.na(Group_Label) & !nzchar(as.character(Group_Label)),
      Group_Label := NA]
    gl_ord <- unique(w[!is.na(Group_Label), .(Group_ID, Group_Label)])
    if (nrow(gl_ord)) {
      setorderv(gl_ord, "Group_ID")
      w[, Group_Label := factor(as.character(Group_Label),
                                levels = unique(as.character(gl_ord$Group_Label)))]
    }
    has_grplab <- any(!is.na(w$Group_Label))
  }

  # Ensure row_split is a factor so user-defined level order is respected
  # by setorderv inside group_data.
  if (!is.null(row_split) && !is.factor(w[[row_split]])) {
    set(w, j = row_split,
        value = factor(as.character(w[[row_split]]),
                       levels = as.character(unique(w[[row_split]]))))
  }

  # Build .label: Statistic for stat rows; the variable label (split /
  # no-row_split) or "rs_lab = X" sub-section text (var mode) for bold
  # rows whose owning header has been suppressed. Visual indent for
  # stat rows is no longer encoded as leading whitespace — it is
  # carried separately as the "indent" token in row_style and applied
  # at render time.
  use_split    <- nest == "split" || is.null(row_split)
  is_bold_data <- w$Var_ID == 0L | w$Row_Style == "bold"
  w[, .label := as.character(Statistic)]
  if (use_split) {
    w[is_bold_data, .label := as.character(Variable)]
  } else {
    w[is_bold_data, .label := paste(rs_lab, "=", as.character(get(row_split)))]
  }

  grp_cols <- if (use_split) {
    c(row_split, if (has_grplab) "Group_Label", ".vkey")
  } else {
    c(if (has_grplab) "Group_Label", ".vkey", row_split)
  }

  # Slim w to just the columns group_data needs (groups, label, data,
  # passthrough metadata for downstream styling).
  meta_pass <- intersect(c("Var_ID", "Row_Style"), names(w))
  keep_cols <- c(grp_cols, ".label", data_cols, meta_pass)
  w <- w[, ..keep_cols]

  hdr_fn <- function(lvl, val, grp) {
    if (!is.null(row_split) && grp == row_split) paste(rs_lab, "=", val)
    else if (grp == ".vkey") vid_to_lbl[[val]]
    else val
  }

  skip_target <- if (use_split) ".vkey" else row_split
  skip_fn <- if (is.null(skip_target)) NULL else
    function(lvl, val, grp, sub) {
      grp == skip_target && any(sub$Var_ID == 0L | sub$Row_Style == "bold")
    }

  res <- group_data(w, groups = grp_cols, shift = TRUE, indent = FALSE,
                    keep_groups = FALSE,
                    passthrough = c(data_cols, meta_pass),
                    skip_na_label = TRUE,
                    header_label_fn = hdr_fn,
                    skip_header_fn = skip_fn,
                    preserve_attrs = FALSE)

  if (nrow(res) == 0L) return(.cttab_empty(data_cols))

  # Compute row indices for each style token, then delegate to
  # format_table() to build the row_style attribute.
  is_hdr  <- attr(res, "is_header")
  is_data <- is_hdr == 0L

  # Bold data rows: single-row variables (logicals) and Observation.
  is_bold <- is_data &
    ((!is.na(res$Row_Style) & res$Row_Style == "bold") |
       (!is.na(res$Var_ID)   & res$Var_ID   == 0L))

  # Map each header row to its grouping column.
  hdr_lvl <- is_hdr
  hdr_lvl[!is_hdr] <- NA_integer_
  grp_at <- grp_cols[hdr_lvl]

  is_rowsplit_hdr <- if (is.null(row_split)) {
    rep(FALSE, nrow(res))
  } else {
    !is.na(grp_at) & grp_at == row_split
  }
  is_vkey_hdr     <- !is.na(grp_at) & grp_at == ".vkey"
  is_grplabel_hdr <- !is.na(grp_at) & grp_at == "Group_Label"

  # Group_Label banners collapse to a plain header when immediately above
  # a bold data row (the bold row already carries the emphasis).
  nxt_is_bold       <- c(is_bold[-1L], FALSE)
  is_grplabel_banner <- is_grplabel_hdr & !nxt_is_bold
  is_grplabel_header <- is_grplabel_hdr &  nxt_is_bold

  # .vkey headers are banners in var-mode, plain headers in split-mode.
  is_vkey_banner <- is_vkey_hdr &  !use_split
  is_vkey_header <- is_vkey_hdr &   use_split

  # Collect row indices per token (union of all row types that carry it).
  idx_bold   <- which(is_bold | is_rowsplit_hdr |
                        is_grplabel_banner | is_grplabel_header |
                        is_vkey_banner     | is_vkey_header)
  idx_bgcol  <- which(is_rowsplit_hdr | is_grplabel_banner | is_vkey_banner)
  idx_span   <- which(is_rowsplit_hdr |
                        is_grplabel_banner | is_grplabel_header |
                        is_vkey_banner     | is_vkey_header)
  idx_indent <- which(is_data & !is_bold)

  # Empty data cells in headers render as "" rather than NA.
  for (col in data_cols) {
    v <- as.character(res[[col]])
    v[is.na(v)] <- ""
    set(res, j = col, value = v)
  }

  out <- data.frame(label = res$.label, stringsAsFactors = FALSE)
  out[data_cols] <- as.list(res)[data_cols]
  out <- format_table(out,
                      bold   = idx_bold,
                      bgcol  = idx_bgcol,
                      span   = idx_span,
                      indent = idx_indent)
  class(out) <- c("cttab", "data.frame")
  out
}
