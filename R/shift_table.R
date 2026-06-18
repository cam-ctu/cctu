#' Baseline x worst-post-baseline shift table
#'
#' Long-format safety shift table. One ordered-factor `value` column supplies
#' the categories (levels best -> worst); all post-baseline visits collapse into
#' a single "worst" category. Grouping variables are placed on the vertical axis
#' (`row_groups`, stacked row blocks) or the horizontal axis (`col_groups`,
#' column spanners), e.g. PARAM down, ARM across.
#'
#' Contract (derivation is assumed done upstream):
#'   * `value` is an ordered factor (levels best -> worst).
#'   * Exactly one baseline row per subject x group; more is an error.
#'   * Every non-baseline row is treated as post-baseline.
#'   * One call is homogeneous in its level system. Mix CTCAE grades and
#'     Low/Normal/High by calling once per system and stacking same-level
#'     parameters via `row_groups`.
#'
#' @param data long data.table: one row per subject-visit(-parameter).
#' @param value ordered-factor column holding the categories (best -> worst).
#' @param id subject identifier column.
#' @param visit visit column.
#' @param bl_value value(s) of `visit` that mark baseline (length >= 1).
#' @param row_groups groups spanning vertically (stacked row blocks).
#' @param col_groups groups spanning horizontally (column spanners).
#' @param worst "max" (default) or "min": which end of the ordering is worst.
#' @param missing_baseline,no_postbaseline labels for absent baseline / post.
#' @param drop_no_postbaseline drop subjects with no post-baseline assessment.
#' @param drop_empty drop empty rows/columns, evaluated on the table margins
#'   within the group layout: a row is (row_groups, baseline) and a column is
#'   (col_groups, worst); it is dropped when its marginal total is zero, so
#'   blocks may become ragged. "none" (default), "rows", "cols", "both".
#' @param pct "none" (default), "row" (\% within baseline, per block) or
#'   "total" (\% of the group block N).
#'
#' @return A \code{data.frame} of class \code{cttab}: a \code{label} column,
#'   the count columns, and a \code{row_style} attribute (via
#'   \code{\link{format_table}}) that bolds the \code{row_groups} banner rows
#'   and indents the baseline rows beneath them for \code{\link{write_table}}.
#'   With no \code{row_groups} the rows are plain.
#' @seealso \code{\link{format_table}}, \code{\link{group_data}},
#'   \code{\link{ae_summary}}, \code{\link{write_table}}
#' @import data.table
#' @export
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   USUBJID = rep(sprintf("S%02d", 1:6), each = 2),
#'   AVISIT  = rep(c("Baseline", "Week 4"), times = 6),
#'   PARAM   = rep(c("ALT", "AST"), each = 6),
#'   ARM     = rep(c("A", "B"), each = 2, length.out = 12),
#'   AVALC   = c("Normal", "High", "Normal", "Normal", "High", "High",
#'               "Normal", "High", "Low", "Normal", "Normal", "High")
#' )
#' dt[, AVALC := factor(AVALC, levels = c("Low", "Normal", "High"),
#'                      ordered = TRUE)]
#' # PARAM as bold row-group banners (down), ARM across, baseline rows indented.
#' shift_table(dt, value = "AVALC", id = "USUBJID", visit = "AVISIT",
#'             bl_value = "Baseline", row_groups = "PARAM", col_groups = "ARM",
#'             pct = "none")
shift_table <- function(data,
                        value,
                        id          = cctu_opt("subjid_string"),
                        visit       = "AVISIT",
                        bl_value    = "Baseline",
                        row_groups  = NULL,
                        col_groups  = NULL,
                        worst       = c("max", "min"),
                        missing_baseline = "Missing",
                        no_postbaseline  = "No post-baseline",
                        drop_no_postbaseline = FALSE,
                        drop_empty = c("none", "rows", "cols", "both"),
                        pct    = c("row", "none", "total")) {

  stopifnot(requireNamespace("data.table", quietly = TRUE))
  worst      <- match.arg(worst)
  drop_empty <- match.arg(drop_empty)
  pct        <- match.arg(pct)

  # data.table NSE column names, bound locally so R CMD check / the
  # object_usage linter don't flag them as undefined globals. These are
  # columns of `dt` / `out`, not variables. (`worst` and `pct` are real
  # arguments and are deliberately excluded.)
  `..bl` <- baseline <- N <- cell <- n_row_margin <- n_col_margin <- NULL # nolint: object_name_linter.

  dt     <- data.table::copy(data.table::as.data.table(data))
  groups <- c(row_groups, col_groups)
  miss   <- setdiff(c(id, visit, value, groups), names(dt))
  if (length(miss)) stop("Columns not found: ", paste(miss, collapse = ", "))

  ## value must already be an ordered factor
  v <- dt[[value]]
  if (!is.ordered(v))
    stop("`value` must be an ordered factor (levels best -> worst).")
  lv <- levels(v)

  ## baseline / post-baseline rows, derived from `visit`
  if (!all(bl_value %in% as.character(dt[[visit]])))
    warning("Not all `bl_value` levels appear in `", visit, "`.")
  dt[, `..bl` := as.character(get(visit)) %in% bl_value]

  ## exactly one baseline row per subject x group
  bcount <- dt[`..bl` == TRUE, .N, by = c(id, groups)]
  if (nrow(bcount) && any(bcount$N > 1L)) {
    nbad <- sum(bcount$N > 1L)
    stop("Found ", nbad, " subject/group combination(s) with more than one ",
         "baseline row (", visit, " %in% bl_value). Expected exactly one per ",
         paste(c(id, groups), collapse = " x "), "; resolve baseline upstream.")
  }

  ## reduce to one row per subject x group: baseline + worst post-baseline
  ## (integer positions into `lv`; value is already an ordered factor)
  worst_pos <- function(x, dir) {
    i <- as.integer(x)
    if (all(is.na(i))) return(NA_integer_)
    i[if (dir == "max") which.max(i) else which.min(i)]
  }
  one_pos <- function(x) {            # single baseline value, or NA
    i <- as.integer(x)
    i <- i[!is.na(i)]
    if (length(i)) i[1L] else NA_integer_
  }

  red <- dt[, {
    bl <- one_pos(.SD[[value]][`..bl`])
    wp <- worst_pos(.SD[[value]][!`..bl`], worst)
    .(baseline = bl, worst = wp)
  }, by = c(id, groups), .SDcols = value]

  ## attach labels (+ Missing / No post-baseline) as ordered display factors
  bl_lv <- c(lv, missing_baseline)
  wp_lv <- if (drop_no_postbaseline) lv else c(lv, no_postbaseline)
  red[, baseline := factor(data.table::fifelse(is.na(baseline), missing_baseline,
                                               lv[baseline]), levels = bl_lv)]
  red[, worst    := factor(data.table::fifelse(is.na(worst), no_postbaseline,
                                               lv[worst]),    levels = wp_lv)]
  if (drop_no_postbaseline) red <- red[worst != no_postbaseline]

  ## full grid incl. zero cells, preserving group factor ordering
  glev <- lapply(groups, function(g) {
    x <- red[[g]]
    if (is.factor(x)) factor(levels(x), levels(x)) else sort(unique(x))
  })
  names(glev) <- groups
  grid <- do.call(data.table::CJ,
                  c(glev,
                    list(baseline = factor(bl_lv, bl_lv),
                         worst    = factor(wp_lv, wp_lv),
                         unique   = TRUE)))

  cnt <- red[, .N, by = c(groups, "baseline", "worst")]
  out <- merge(grid, cnt, by = c(groups, "baseline", "worst"), all.x = TRUE)
  out[is.na(N), N := 0L]

  ## drop empty rows / columns on the laid-out margins, within the group block:
  ## a row is (row_groups, baseline), a column is (col_groups, worst); drop it
  ## when its marginal total is zero. Blocks may end up ragged.
  if (drop_empty %in% c("rows", "both")) {
    out[, n_row_margin := sum(N), by = c(row_groups, "baseline")]
    out <- out[n_row_margin > 0L]
    out[, n_row_margin := NULL]
  }
  if (drop_empty %in% c("cols", "both")) {
    out[, n_col_margin := sum(N), by = c(col_groups, "worst")]
    out <- out[n_col_margin > 0L]
    out[, n_col_margin := NULL]
  }
  if (drop_empty != "none") {
    out[, baseline := droplevels(baseline)]
    out[, worst    := droplevels(worst)]
  }

  ## percentages: the denominator is the by-group total, folded into the `:=`
  ## so no temporary `denom` column is needed.
  if (pct != "none") {
    pct_by <- if (pct == "row") c(groups, "baseline") else groups
    out[, pct := {
      d <- sum(N)            # by-group denominator (scalar within the group)
      if (d > 0) 100 * N / d else NA_real_
    }, by = pct_by]
  }

  data.table::setkeyv(out, c(groups, "baseline", "worst"))

  ## wide: row_groups + baseline  ~  col_groups + worst
  lhs  <- paste(c(row_groups, "baseline"), collapse = " + ")
  rhs  <- paste(c(col_groups, "worst"),    collapse = " + ")
  form <- stats::as.formula(paste(lhs, "~", rhs))
  if (pct == "none") {
    wide <- data.table::dcast(out, form, value.var = "N", fill = 0L)
  } else {
    out[, cell := sprintf("%d (%.1f)", N, pct)]
    wide <- data.table::dcast(out, form, value.var = "cell", fill = "0 (0.0)")
  }

  ## Fold row_groups into bold banner rows sitting above their indented
  ## baseline rows (via group_data), then stamp the row styles. With no
  ## row_groups there is nothing to group, so rows stay plain.
  .shift_style(wide, row_groups)
}

# Style a shift_table wide cross-tab into a rendering-ready cttab: a `label`
# first column plus the count columns, with a `row_style` attribute. When
# `row_groups` is given, group_data() inserts a blank banner row per group
# level (count columns set to NA) and folds the baseline categories beneath it;
# those banners are bolded and the baseline rows indented.
#' @keywords internal
#' @import data.table
.shift_style <- function(wide, row_groups) {
  if (is.null(row_groups)) {
    data.table::setnames(wide, "baseline", "label")
    data_cols <- setdiff(names(wide), "label")
    out <- data.frame(label = as.character(wide[["label"]]),
                      stringsAsFactors = FALSE)
    for (cc in data_cols) out[[cc]] <- as.character(wide[[cc]])
    return(format_table(out))
  }

  g <- group_data(wide, groups = row_groups, shift_to = "baseline",
                  indent = TRUE)
  data.table::setnames(g, "baseline", "label")
  data_cols <- setdiff(names(g), "label")
  # group_data() blanks the non-group/non-shift columns on the banner rows.
  banner <- is.na(g[[data_cols[1L]]])

  out <- data.frame(label = as.character(g[["label"]]),
                    stringsAsFactors = FALSE)
  for (cc in data_cols) {
    v <- as.character(g[[cc]])
    v[is.na(v)] <- ""
    out[[cc]] <- v
  }
  format_table(out, bold = which(banner), indent = which(!banner))
}
