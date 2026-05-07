#' Stamp row-style metadata on a table for `write_table()`.
#'
#' Generic helper that attaches a `row_style` attribute to a data.frame
#' or matrix so it can be rendered as a styled Word table by
#' [write_table()] / [styled_table()]. Each argument takes integer row
#' indices for the rows that should carry the corresponding token; the
#' helper assembles them into the per-row `;`-joined character vector
#' that the renderer consumes.
#'
#' Tokens (the renderer's vocabulary): `"bold"` (bold text), `"bgcol"`
#' (grey background — typically a banner row), `"span"` (first cell
#' spans all columns — typically a header / banner row), `"indent"`
#' (visual indent — typically a stat row sitting under a header).
#' A row may carry any combination, e.g. a banner row appears in
#' `bold`, `bgcol`, and `span`. Token order in the assembled string is
#' fixed: `bold;bgcol;span;indent;col`.
#'
#' @param x A data.frame or matrix.
#' @param bold,bgcol,span,indent Integer vectors of row indices (1-based)
#'   that should carry the corresponding style. `NULL` (default) means
#'   no rows of that style. Indices outside `[1, nrow(x)]` or `NA` are
#'   rejected with an error.
#' @param bgcol_color A color for the background of rows in `bgcol`. Accepts
#'   any R color name (e.g. `"red"`) or a 6-digit hex string (e.g. `"FF0000"`).
#'   `NULL` (default) uses the document default grey (`d3d3d3`).
#' @param col Integer vector of row indices whose text should be coloured.
#' @param col_color A color for the text of rows in `col`. Same format as
#'   `bgcol_color`. Required when `col` is non-`NULL`.
#'
#' @return `x`, unchanged in shape, with `attr(x, "row_style")` set to
#'   a character vector of length `nrow(x)`.
#' @seealso [styled_table()], [write_table()], [cttab_format()].
#' @export
#' @examples
#' df <- data.frame(label = c("Group A", "n", "mean"), value = c("", "10", "3.4"))
#' df <- format_table(df, bold = 1, bgcol = 1, span = 1, indent = 2:3)
#' attr(df, "row_style")
#' # [1] "bold;bgcol;span" "indent"          "indent"
#'
#' # Custom background and text colors
#' df2 <- format_table(df, bgcol = 1, bgcol_color = "steelblue", col = 2:3, col_color = "red")
#' attr(df2, "row_style")
format_table <- function(x,
                         bold        = NULL,
                         bgcol       = NULL,
                         bgcol_color = NULL,
                         span        = NULL,
                         indent      = NULL,
                         col         = NULL,
                         col_color   = NULL) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("`x` must be a data.frame or matrix.")
  }
  n <- nrow(x)

  validate <- function(idx, name) {
    if (is.null(idx)) return(integer(0))
    if (!is.numeric(idx)) {
      stop("`", name, "` must be a numeric vector of row indices.")
    }
    idx <- as.integer(idx)
    if (anyNA(idx)) {
      stop("`", name, "` contains NA.")
    }
    if (length(idx) > 0L && (min(idx) < 1L || max(idx) > n)) {
      stop("`", name, "` contains indices outside [1, ", n, "].")
    }
    unique(idx)
  }

  to_hex <- function(color, name) {
    if (is.null(color)) return(NULL)
    rgb_vals <- tryCatch(
      grDevices::col2rgb(color)[, 1],
      error = function(e) stop("`", name, "` is not a valid color: ", color)
    )
    toupper(sprintf("%02X%02X%02X", rgb_vals[1], rgb_vals[2], rgb_vals[3]))
  }

  bold_i   <- validate(bold,   "bold")
  bgcol_i  <- validate(bgcol,  "bgcol")
  span_i   <- validate(span,   "span")
  indent_i <- validate(indent, "indent")
  col_i    <- validate(col,    "col")

  bgcol_hex <- to_hex(bgcol_color, "bgcol_color")
  col_hex   <- to_hex(col_color,   "col_color")

  if (length(col_i) > 0L && is.null(col_hex)) {
    warning("`col` rows specified but `col_color` is NULL; text color not applied.")
    col_i <- integer(0)
  }

  bgcol_token <- if (!is.null(bgcol_hex)) paste0("bgcol:", bgcol_hex) else "bgcol"
  col_token   <- if (!is.null(col_hex))   paste0("col:",   col_hex)   else NULL

  rs <- character(n)
  add_token <- function(rs, idx, token) {
    if (length(idx) == 0L) return(rs)
    cur <- rs[idx]
    rs[idx] <- ifelse(nzchar(cur), paste(cur, token, sep = ";"), token)
    rs
  }
  rs <- add_token(rs, bold_i,   "bold")
  rs <- add_token(rs, bgcol_i,  bgcol_token)
  rs <- add_token(rs, span_i,   "span")
  rs <- add_token(rs, indent_i, "indent")
  rs <- add_token(rs, col_i,    col_token)

  attr(x, "row_style") <- rs
  x
}
