#' Function to write a table into xml format in the correct directory, and
#' record the calling program in the meta_table.
#'
#' @param x the data.frame or table to be saved in xml format
#' @param number the number used as a suffix in the output filename, and to
#'   link to the meta_table. Default is to use the value in the cctu_env
#'   package environment that is set within \code{\link{attach_pop}}.
#' @param heading character vector of column titles. Defaults to the colnames
#'   of x
#' @param na_to_empty logical, if true then any NA values will be written as
#'   empty strings. Defaults to false.
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at
#'   the end. Defaults to \code{TRUE}.
#' @param directory where to save the table within path or current working
#'   directory. Defaults to \code{file.path(cctu_opt("output"), "Core")}.
#' @param verbose logical to print information on changes to the global
#'   environment or external files. Defaults to \code{options()$verbose}.
#' @param footnote character vector, can be used to add footnotes. Use
#'   \code{@ref\{number\}} anywhere in the text to insert a clickable
#'   cross-reference to another table or figure by its number, e.g.
#'   \code{"See @ref{1.1} for details"} renders as \code{"See Table 1.1 for
#'   details"} with "Table 1.1" linking to that table's heading.
#' @param spanner_sep \code{NULL} (default) for a single-row header,
#'   byte-for-byte as before. A separator string (e.g. \code{"_"}) builds a
#'   two-level header by splitting each data-column name on its first
#'   occurrence of the separator: the text before becomes a group spanner
#'   above, the text after the leaf label below. Column 1 is always the row
#'   label and is never split. Useful for \code{\link{shift_table}} output
#'   with \code{col_groups}, whose columns are named \code{group_leaf}.
#'
#' @details
#' For the plain (non-styled) path, variable names and values are replaced
#' by variable labels and value labels respectively before writing.
#' For a \code{cttab} input the labels were already baked in upstream by
#' \code{\link{cttab_format}}. Set \code{cctu_options(na_to_empty = TRUE)}
#' (or \code{options(cctu_na_to_empty = TRUE)}) to make NA-to-empty the
#' global default.
#'
#' @return writes an xml version of the input data to file table_number.xml.
#'  Records the calling program in the meta_table. No return object.
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{detect_invalid_utf8}}
#' \code{\link{remove_invalid_utf8}}
#' \code{\link{lab2val}} \code{\link{var_lab}} \code{\link{val_lab}}
#'  \code{\link{apply_macro_dict}}

write_table <- function(x,
                        number = cctu_env$number,
                        heading = NULL,
                        na_to_empty = cctu_opt("na_to_empty"),
                        clean_up = TRUE,
                        directory = file.path(cctu_opt("output"), "Core"),
                        verbose = options()$verbose,
                        footnote = NULL,
                        spanner_sep = NULL) {
  calling_prog <- cctu_env$parent[1] # get_file_name()
  if (is.null(calling_prog)) {
    warning("Unable to identify the code file that created table", number)
    calling_prog <- "Missing"
  }
  add_program(number, calling_prog)
  if (!is.null(footnote)) {
    add_footnote(number, footnote)
  }

  # Empty / shapeless input: replace with a one-cell "No Data" sentinel.
  # Runs before the cttab adapter so an empty cttab also degrades to this
  # rather than producing an empty styled table.
  if (is.null(dim(x)) || dim(x)[1] == 0 || dim(x)[2] == 0) {
    x <- setNames(
      data.frame("No Data", stringsAsFactors = FALSE),
      ""
    )
  }

  # cttab adapter: a raw long-format cttab gets reshaped into a
  # styled-table data.frame before dispatch so the user-visible API
  # (write_table(my_cttab)) keeps working.
  if (inherits(x, "cttab") && !is_formatted_cttab(x)) {
    x <- cttab_format(x)
  }

  # Dispatch on row_style: a styled table (cttab output, or anything the
  # user stamped via format_table()) goes through styled_table(); plain
  # data.frames / matrices go through table_data().
  if (!is.null(attr(x, "row_style"))) {
    output_string <- styled_table(x, spanner_sep)
  } else {
    output_string <- table_data(x, heading, na_to_empty, spanner_sep)
  }

  file_name <- file.path(directory, paste0("table_", number, ".xml"))

  cat(output_string, file = file_name, append = FALSE)
  if (verbose) {
    cat("\n", file_name, "created.\n")
  }

  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
}


#' Build the `<thead>` fragment, optionally with two-level spanner headers.
#'
#' Shared by both the plain (\code{table_data}) and styled
#' (\code{\link{styled_table}}) write paths so the header logic lives in one
#' place. With \code{spanner_sep = NULL} it emits today's single-row header,
#' byte-for-byte. With a separator string it splits each data-column name on
#' its first occurrence of \code{spanner_sep} into a (group, leaf) pair and
#' emits a two-row header: an arm/group spanner row above the leaf row, with
#' the first (stub) column vertically merged across both rows.
#'
#' @param headings character vector of column headings. The first element is
#'   the row-label/stub and is never split.
#' @param spanner_sep \code{NULL} (single-row header) or a separator string
#'   used to split the data-column names.
#' @return a character scalar: the full \code{<thead>...</thead>\\n} fragment.
#' @keywords internal
build_thead <- function(headings, spanner_sep = NULL) {
  esc <- remove_xml_specials

  # Single-row header: default behaviour, or nothing to group (stub only).
  if (is.null(spanner_sep) || length(headings) < 2L) {
    th <- paste0("<th>", esc(headings), "</th>", collapse = "")
    return(paste0("<thead>\n<tr>", th, "</tr>\n</thead>\n"))
  }

  stub      <- headings[1L]
  data_head <- headings[-1L]

  # Split each data-column name on its first occurrence of `spanner_sep`.
  # Before = group label, after = leaf label. No separator => empty group.
  pos   <- regexpr(spanner_sep, data_head, fixed = TRUE)
  has   <- pos > 0L
  group <- ifelse(has, substr(data_head, 1L, pos - 1L), "")
  leaf  <- ifelse(has,
                  substr(data_head, pos + nchar(spanner_sep), nchar(data_head)),
                  data_head)

  # Run-length-encode the group labels and build the two header rows in
  # parallel. Consecutive equal non-empty groups collapse into one spanning
  # cell above their leaf cells. Ungrouped columns (empty group) instead
  # vertically merge across both rows, carrying their label in the top cell so
  # it sits beside the leaf row (bottom alignment comes from the XSLT). A
  # right-border (`rborder`) separates two adjacent non-empty groups, stamped
  # on both the spanner cell and the group's rightmost leaf cell so the rule
  # is continuous across both header rows.
  runs <- rle(group)
  top_cells <- character(0)
  bot_cells <- character(0)
  idx <- 0L
  for (i in seq_along(runs$lengths)) {
    k    <- runs$lengths[i]
    g    <- runs$values[i]
    cols <- seq.int(idx + 1L, idx + k)
    if (nzchar(g)) {
      rb <- if (i < length(runs$values) && nzchar(runs$values[i + 1L])) {
        " rborder='1'"
      } else {
        ""
      }
      top_cells <- c(top_cells,
                     paste0("<th colspan='", k, "'", rb, ">", esc(g), "</th>"))
      lc    <- paste0("<th>", esc(leaf[cols]), "</th>")
      lc[k] <- paste0("<th", rb, ">", esc(leaf[cols][k]), "</th>")
      bot_cells <- c(bot_cells, lc)
    } else {
      top_cells <- c(top_cells,
                     paste0("<th vmerge='restart'>", esc(leaf[cols]), "</th>"))
      bot_cells <- c(bot_cells, rep("<th vmerge='cont'></th>", k))
    }
    idx <- idx + k
  }

  # Stub corner: vMerge across both rows, label in the (restart) top cell.
  top <- paste0("<tr><th vmerge='restart'>", esc(stub), "</th>",
                paste0(top_cells, collapse = ""), "</tr>\n")
  bottom <- paste0("<tr><th vmerge='cont'></th>",
                   paste0(bot_cells, collapse = ""), "</tr>\n")

  paste0("<thead>\n", top, bottom, "</thead>\n")
}


#' @keywords internal
#'
remove_xml_specials <- function(x) {
  # Remove non-UTF-8 here or the gsub will fail for non-UTF-8 characters
  # Ref: https://blog.r-project.org/2022/06/27/why-to-avoid-%5Cx-in-regular-expressions/
  x <- rm_invalid_utf8(x)
  x <- gsub("&(?!#\\d+;)", "&amp;\\1", x, perl = TRUE)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  x <- gsub('"', "&quot;", x)
  x <- gsub("'", "&apos;", x)
  x
}



# For normal
#' @keywords internal
#' @importFrom utils capture.output
table_data <- function(x, heading = NULL, na_to_empty = FALSE,
                       spanner_sep = NULL) {
  if (!is.null(heading) && ncol(x) != length(heading)) {
    stop("Heading should have the same length as the number of columns")
  }

  # Resolve column headings: explicit `heading` wins; otherwise fall back to
  # variable labels where present, else colnames.
  if (is.null(heading)) heading <- colnames(x)
  with_varlab <- vapply(x, has_label, logical(1))
  if (any(with_varlab)) {
    heading[with_varlab] <- unlist(var_lab(x)[with_varlab])
  }

  # Matrix path: if rownames are non-default, surface them as a leading
  # "Variables" column. NROW() guards against zero-row matrices where
  # rownames(x) is NULL.
  if (inherits(x, "matrix") && NROW(x) > 0L) {
    rn <- as.character(rownames(x))
    if (length(rn) && any(rn != as.character(seq_len(nrow(x))))) {
      heading <- c("Variables", heading)
      x <- data.frame(row_nam = rn, x, row.names = NULL)
    }
  }

  # Variable values to labels if has value
  x <- lab2val(x)

  if (inherits(x, "data.frame")) {
    utf8_check <- detect_invalid_utf8(x)
    utf8_check_cap <- capture.output(utf8_check)
    utf8_check_cap <- paste(utf8_check_cap, "\n", sep = "")
    if (nrow(utf8_check)) {
      warning("Invalid non-UTF8 characters found\n", utf8_check_cap, "\n")
    }
  }


  # Table header (single- or two-row depending on `spanner_sep`)
  thead <- build_thead(heading, spanner_sep)

  # Table body. apply() coerces each column to character via the matrix
  # roundtrip; callers are expected to pre-format numerics / dates if they
  # care about the rendering.
  td <- apply(x, 2, function(c) {
    if (na_to_empty) {
      c <- ifelse(is.na(c), "", c)
    }
    paste0("<td>", remove_xml_specials(c), "</td>")
  })
  if (is.null(dim(td))) {
    td <- paste0(td, collapse = "")
  } else {
    td <- apply(td, 1, paste0, collapse = "")
  }
  td <- paste0("<tr>", td, "</tr>\n", collapse = "")
  tbody <- paste0("<tbody>\n", td, "</tbody>\n")

  # Table output
  paste("\n<table>\n", thead, tbody, "</table>\n")
}


#' Render a styled table to Word-flavoured XML.
#'
#' Generic XML renderer for any data.frame / matrix that carries a
#' \code{row_style} attribute. \code{row_style} is a per-row character
#' vector of \code{;}-joined tokens drawn from
#' \code{\{"bold", "bgcol", "span", "indent", "col"\}}; \code{""} marks a
#' plain stat row. Tokens are passed through verbatim into the
#' \code{style} attribute on each \code{<td>}, where the XSLT in
#' \code{inst/assets/document.xslt} (used by \code{\link{write_docx}})
#' interprets them.
#'
#' Two input shapes are accepted:
#' \itemize{
#'   \item a \code{data.frame} with a \code{label} column (the first
#'     column of the rendered table) plus one column per data column;
#'   \item a \code{matrix} whose row-names act as labels.
#' }
#'
#' @param x A data.frame (with a \code{label} column) or matrix.
#' @param spanner_sep \code{NULL} for a single-row header, or a separator
#'   string passed to \code{\link{build_thead}} to build two-level spanner
#'   headers from the data-column names.
#' @return A character scalar containing the table's XML fragment.
#' @keywords internal
styled_table <- function(x, spanner_sep = NULL) {
  rowclass <- attr(x, "row_style")
  if (is.null(rowclass)) {
    stop("`styled_table()` requires a 'row_style' attribute on `x`.")
  }

  if (is.matrix(x)) {
    # Matrix path: prepend rownames as a label column
    rl     <- rownames(x)
    xm     <- cbind("Variable" = rl, x)
    hd_nam <- colnames(xm)
  } else if ("label" %in% names(x)) {
    # data.frame with a label column: rename it to "Variable"
    rl      <- as.character(x$label)
    dc      <- setdiff(names(x), "label")
    xm      <- as.matrix(cbind(Variable = rl, x[, dc, drop = FALSE]))
    mode(xm) <- "character"
    xm[is.na(xm)] <- ""
    hd_nam  <- c("Variable", dc)
  } else {
    # Plain data.frame: use columns as-is, no extra label column
    xm      <- as.matrix(x)
    mode(xm) <- "character"
    xm[is.na(xm)] <- ""
    hd_nam  <- colnames(x)
  }

  xm[] <- apply(xm, 2, remove_xml_specials)

  # Table header (single- or two-row depending on `spanner_sep`)
  thead <- build_thead(hd_nam, spanner_sep)

  # Per-row CSS class string: tokens from row_style + "firstleft".
  # The XSLT (inst/assets/document.xslt) reads the row's style attribute
  # from `td[1]/@style`, so we only stamp the first cell of each row;
  # the remaining cells inherit the row's behaviour from there.
  al <- "firstleft"
  cls_str <- ifelse(nzchar(rowclass),
                    paste(rowclass, al, sep = ";"),
                    al)
  cls <- matrix("", nrow(xm), ncol(xm))
  cls[, 1] <- paste0(" style='", cls_str, "'")

  td <- paste0("<td", cls, ">", as.matrix(xm), "</td>")
  dim(td) <- dim(xm)
  td <- apply(td, 1, paste0, collapse = "")
  td <- paste0("<tr>", td, "</tr>\n", collapse = "")
  tbody <- paste0("<tbody>\n", td, "</tbody>\n")

  paste("\n<table>\n", thead, tbody, "</table>\n")
}
