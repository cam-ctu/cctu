#' Function to write a table into xml format in the correct directory, and edit
#' TableofTables
#'
#' @inheritParams write_ggplot
#' @param x the data.frame or table to be saved in xml format
#' @param heading character vector of column titles. Defaults to the colnames
#' of x
#' @param na_to_empty logical, if true then any NA values will be written as
#'  empty strings. Defaults to false.
#'
#' @details
#' Variable names and values will be replace by variable labels and value
#' labels respectively if available before writing the data.
#' Use \code{options(cctu_na_to_empty = TRUE)} to write NA values will be
#' written as empty strings globally.
#'
#' @return writes an xml version of the input data to file table_number.xml.
#'  Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{detect_invalid_utf8}}
#' \code{\link{remove_invalid_utf8}}
#' \code{\link{lab2val}} \code{\link{var_lab}} \code{\link{val_lab}}
#'  \code{\link{apply_macro_dict}}
#' @importFrom magrittr %>% %<>%

write_table <- function(x,
                        number = cctu_env$number,
                        heading = NULL,
                        na_to_empty = getOption("cctu_na_to_empty",
                          default = FALSE
                        ),
                        clean_up = TRUE,
                        directory = file.path(
                          getOption("cctu_output",
                            default = "Output"
                          ),
                          "Core"
                        ),
                        verbose = options()$verbose,
                        footnote = NULL) {
  calling_prog <- cctu_env$parent[1] # get_file_name()
  if (is.null(calling_prog)) {
    warning("Unable to identify the code file that created table", number)
    calling_prog <- "Missing"
  }
  add_program(number, calling_prog)
  if (!is.null(footnote)) {
    add_footnote(number, footnote)
  }

  if (is.null(dim(x)) || dim(x)[1] == 0 || dim(x)[2] == 0) {
    x <- data.frame(" " = "No Data")
    colnames(x) <- ""
  }


  if (inherits(x, "cttab")) {
    output_string <- table_cttab(x)
  } else {
    output_string <- table_data(x, heading, na_to_empty)
  }

  # directory %<>% normalizePath %>% final_slash
  file_name <- file.path(directory, paste0("table_", number, ".xml"))

  cat(output_string, file = file_name, append = FALSE)
  if (verbose) {
    cat("\n", file_name, "created.\n")
  }

  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
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
table_data <- function(x, heading = NULL, na_to_empty = FALSE) {
  if (!is.null(heading) && ncol(x) != length(heading)) {
    stop("Heading should have the same length as the number of columns")
  }

  if (!is.null(heading)) {
    for (i in seq_along(heading)) {
      var_lab(x[[i]]) <- heading[i]
    }
  } else {
    heading <- colnames(x)
  }

  check <- as.character(rownames(x)) != as.character(seq_len(nrow(x)))
  if (inherits(x, "matrix") && any(check)) {
    heading <- c("Variables", heading)
    x <- data.frame(row_nam = rownames(x), x, row.names = NULL)
  }

  # Variable names to labels if no variable label
  with_varlab <- sapply(x, has_label)
  if (any(with_varlab)) {
    heading[with_varlab] <- unlist(var_lab(x)[with_varlab])
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


  # Table header
  th <- paste0("<th>", remove_xml_specials(heading), "</th>", collapse = "")
  th <- paste0("<tr>", th, "</tr>\n")
  thead <- paste0("<thead>\n", th, "</thead>\n")

  # Table body
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


# For cttab class
#' @keywords internal
#'
table_cttab <- function(x) {
  rl <- rownames(x)
  rowclass <- attr(x, "position")

  if (inherits(x, "matrix")) {
    x <- cbind("Variable" = rl, x)
    al <- "firstleft"
    hd_nam <- colnames(x)
  } else {
    # Variable names to labels if no variable label
    with_varlab <- sapply(x, has_label)
    for (i in names(x)[!with_varlab]) {
      var_lab(x[[i]]) <- i
    }
    hd_nam <- unlist(var_lab(x))
    # Variable values to labels if has value
    x <- lab2val(x)
    al <- ""
    x[is.na(x)] <- ""
  }

  x[] <- apply(x, 2, remove_xml_specials)

  # Table header
  th <- paste0("<th>", remove_xml_specials(hd_nam), "</th>", collapse = "")
  th <- paste0("<tr>", th, "</tr>\n")
  thead <- paste0("<thead>\n", th, "</thead>\n")

  # Table body
  # Add class
  cls <- matrix(NA, nrow(x), ncol(x))
  rowclass <- sapply(
    seq_along(rowclass),
    function(x) {
      switch(as.character(rowclass[x]),
        "0" = "bold;bgcol;span",
        "1" = "bold",
        "2" = "bold;span",
        "3" = "indent",
        ""
      )
    }
  )

  cls[, 1] <- paste0(" style='", paste(rowclass, al, sep = ";"), "'")
  cls[is.na(cls)] <- ""

  td <- paste0("<td", cls, ">", as.matrix(x), "</td>")
  dim(td) <- dim(x)
  td <- apply(td, 1, paste0, collapse = "")
  td <- paste0("<tr>", td, "</tr>\n", collapse = "")
  tbody <- paste0("<tbody>\n", td, "</tbody>\n")

  # Table output
  paste("\n<table>\n", thead, tbody, "</table>\n")
}
