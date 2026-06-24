#' The meta_table: get, set and an example
#'
#' The meta_table is the index of every output in a report - one row per table,
#' figure or text item. \code{set_meta_table} stores it in the internal
#' \code{cctu} environment (after cleaning); \code{get_meta_table} returns the
#' stored copy; and \code{meta_table_example} is an example data frame showing
#' the expected structure.
#'
#' @param meta_table a data.frame to be set as the meta_table internal object
#' @return \code{get_meta_table} returns the internal object;
#'  \code{set_meta_table} invisibly returns the previous version;
#'  \code{meta_table_example} is a data frame.
#' @details \code{set_meta_table(NULL)} will remove the meta_table.

#' @describeIn get_meta_table  gets a copy of the internal object
#' @export
get_meta_table <- function() {
  cctu_env$meta_table
}

#' @describeIn get_meta_table sets the internal object to the argument provided,
#'  whilst carrying out some cleaning
#' @export
set_meta_table <- function(meta_table) {
  old <- cctu_env$meta_table
  if (is.null(meta_table)) {
    rm("meta_table", envir = cctu_env)
  } else {
    meta_table <- clean_meta_table(meta_table)
    cctu_env$meta_table <- meta_table
  }
  invisible(old)
}



#' @keywords internal
add_program <- function(number, calling_prog) {
  meta_table <- get_meta_table()
  if (!("number" %in% names(meta_table))) {
    stop("Need to have 'number' column in meta_table")
  }
  if (!("program" %in% names(meta_table))) {
    warning("Need to have 'program' column in meta_table", immediate. = TRUE)
  }
  index <- match(number, meta_table$number)
  meta_table[index, "program"] <- calling_prog
  set_meta_table(meta_table)
}

#' @keywords internal
add_footnote <- function(number, footnote) {
  meta_table <- get_meta_table()
  if (!("number" %in% names(meta_table))) {
    stop("Need to have 'number' column in meta_table")
  }
  index <- match(number, meta_table$number)
  original_footnote <- meta_table[index, "footnote2"]
  if (is_empty(original_footnote)) {
    meta_table[index, "footnote2"] <- paste(footnote, collapse = "\n")
  } else {
    meta_table[index, "footnote2"] <- paste(c(original_footnote, footnote),
      collapse = "\n"
    )
  }
  set_meta_table(meta_table)
}



#' Resolve cross-references in footnote text
#'
#' Replaces \code{@ref\{number\}} tokens in footnote strings with an
#' \code{<xref>} element that carries the target Word bookmark name and a
#' display label (e.g. \code{"Table 1.1"} or \code{"Figure 2.3"}). The XSLT in
#' \code{inst/assets/document.xslt} turns each \code{<xref>} into a clickable
#' \code{REF} field pointing at the bookmark that the heading template stamps
#' on every table/figure (\code{<titletype>_<digits-of-number>}). Using a
#' \code{REF} field means the reference tracks the heading number when fields
#' are updated in Word, rather than being frozen text. The label
#' prefix and bookmark are derived from the \code{item} column of the meta_table
#' (\code{"table"} -> \code{"Table"}, \code{"figure"} -> \code{"Figure"}; text
#' items are rendered as \code{"Table"} headings by the XSLT, so they share that
#' bookmark prefix). If a referenced number is not found in the meta_table a
#' warning is issued and the token is replaced with \code{"[ref: <number>]"}.
#'
#' @param footnote character vector of footnote strings, each potentially
#'   containing one or more \code{@ref\{number\}} tokens.
#' @param meta_table a data frame with at least \code{number} and \code{item}
#'   columns, as returned by \code{\link{get_meta_table}}.
#' @return character vector the same length as \code{footnote} with all
#'   \code{@ref\{\}} tokens replaced by \code{<xref>} elements.
#' @keywords internal
resolve_footnote_refs <- function(footnote, meta_table = get_meta_table()) {
  pattern <- "@ref\\{([^}]+)\\}"
  vapply(footnote, function(fn) {
    if (!grepl(pattern, fn, perl = TRUE)) {
      return(fn)
    }
    matches <- regmatches(fn, gregexpr(pattern, fn, perl = TRUE))[[1]]
    nums <- sub(pattern, "\\1", matches, perl = TRUE)
    for (i in seq_along(matches)) {
      idx <- match(nums[i], meta_table$number)
      if (is.na(idx)) {
        warning(sprintf("@ref{%s} not found in meta_table", nums[i]),
                call. = FALSE)
        replacement <- sprintf("[ref: %s]", nums[i])
      } else {
        # Bookmark prefix must match the `titletype` the XSLT stamps on each
        # heading; text items are rendered as 'Table' headings there too.
        item_type <- tolower(meta_table$item[idx])
        prefix <- switch(item_type,
          table  = "Table",
          figure = "Figure",
          text   = "Table",
          tools::toTitleCase(item_type)
        )
        # Bookmark name mirrors document.xslt: <titletype>_<digits-of-number>.
        bookmark <- paste0(prefix, "_", gsub("[. ]", "", nums[i]))
        label <- paste(prefix, nums[i])
        replacement <- sprintf(
          "<xref bookmark=\"%s\" label=\"%s\"/>", bookmark, label
        )
      }
      fn <- sub(matches[i], replacement, fn, fixed = TRUE)
    }
    fn
  }, character(1), USE.NAMES = FALSE)
}


#' @keywords internal
#'

clean_meta_table <- function(meta_table) {
  op <- options()
  options(stringsAsFactors = FALSE)
  columns_needed <- c(
    "section", "title", "subtitle", "number", "population",
    "orientation", "margin", "program", "item", "footnote1", "footnote2",
    "fontsize"
  )

  # makes empty columns are characters - read_excel will make them logicals.
  meta_table <- lapply(meta_table, as.character) |> data.frame()

  if (!("number" %in% names(meta_table))) {
    stop("Need to have 'number' column in meta_table")
  } else {
    meta_table$number <- gsub("\\s", "", meta_table$number)
    index <- meta_table$number |>
      as.character() |>
      order_dewey()
    meta_table <- meta_table[index, ]
    pmat <- pmatch(names(meta_table), columns_needed)
    meta_table <- subset(meta_table, !is.na(meta_table$number) &
                           meta_table$number != "", select = !is.na(pmat))
  }

  if (!("item" %in% names(meta_table))) {
    warning("Need to have 'item' column meta_table", immediate. = TRUE)
  }




  pmat <- pmatch(names(meta_table), columns_needed)
  names(meta_table) <- columns_needed[pmat]

  n <- nrow(meta_table)

  extra_cols <- columns_needed[-pmat]
  if (length(extra_cols)) {
    x <- matrix("", nrow = n, ncol = length(extra_cols))
    x <- as.data.frame(x)
    names(x) <- extra_cols
    meta_table <- cbind(meta_table, x)
  }

  meta_table$population <- gsub("\\s", "", meta_table$population)

  if ("orientation" %in% extra_cols) {
    meta_table$orientation <- "portrait"
  }

  item_index <- pmatch(meta_table$item |> tolower(),
    c("table", "figure", "text"),
    duplicates.ok = TRUE
  )
  if (any(is.na(item_index))) {
    warning("invalid values for 'item' converted to 'table'",
      immediate. = TRUE
    )
    item_index <- ifelse(is.na(item_index), 1, item_index)
  }
  meta_table$item <- c("table", "figure", "text")[item_index]

  orient_index <- pmatch(meta_table$orientation |> tolower(),
    c("portrait", "landscape"),
    duplicates.ok = TRUE
  )
  if (any(is.na(orient_index))) {
    warning("invalid values for 'orientation' converted to 'portrait'",
      immediate. = TRUE
    )
    orient_index <- ifelse(is.na(orient_index), 1, orient_index)
  }
  meta_table$orientation <- c("portrait", "landscape")[orient_index]
  options(op)
  meta_table
}
