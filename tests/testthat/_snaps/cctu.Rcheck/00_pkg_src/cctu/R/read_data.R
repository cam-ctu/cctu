#' Automatic reading in data from a meta-table of external data sets.
#'
#' @param x character string or data.frame.  If it is a character then it is
#' the name of the object to be created, and referenced within the data to find
#'  the file path. If it is a dataframe then read_data is repeated across all
#'  the rows of the data.frame.
#' @param data_table data frame containing the meta-table of file paths of the
#'  external data files, and their desired R object names.
#' @param fun the function to be used to read in the data file. If unspecified
#' it picks up file extensions ".xsl" and ".xslx" to use \code{readxl::read_xls}
#'  and \code{readxl::read_xlsx}, otherwise uses \code{read.csv}. This could
#'  actually be any function applied to the file path character string that is
#'   extracted from \code{data_table}, but a warning is issued if the function
#'    name does not contain "read".
#' @param frame Environment in which an object with name given by \code{x} is
#' created. Default is parent.frame(). Or if NULL the data read in is returned
#'  with no assignment.
#' @param name_variable character string giving the variable name within
#' \code{data} that has the object names to be referenced. Defaults to "name".
#' @param file_variable character string giving the variable name within
#' \code{data} that has the file names to be referenced. Defaults to "file".
#' @param clean_names_option logical to apply the \code{\link{clean_names}}
#'  function internally. Defaults to \code{FALSE} for compatibility with
#'  \code{\link{apply_macro_dict}}.
#' @param remove_blank_rows_cols_option logical to apply the
#' \code{\link{remove_blank_rows_cols}} function internally. Defaults to
#' \code{FALSE} for compatibility with \code{\link{apply_macro_dict}}.
#' @param ... other arguments to supply to \code{fun}.
#'
#' @return \code{read_data} assigns or returns a data frame reading in data
#' from an external file
#'
#' @details The idea is to improve the tracibility of reading in external data.
#' This should be used in two steps: create a meta-table in R that has a minimum
#'  of 2 columns, one with the name of the R data.frame to be created, and the
#'  other giving the file path to the external data; use \code{read_data} as a
#'  wrapper to read in the data as specified. This ends up with less code, and
#'  allows an table of extenral data and associated meta-data to
#' be easily produced using \code{data_table_summary}. If options("verbose") is
#'  \code{TRUE} then \code{read_data} will display messages describing what
#'  objects have been created.
#'
#' This is a generic method with methods defined for a character string, and a
#' data.frame. The former just reads in one data.frame, the latter reads in all
#'  the data.frames specified.
#'
#' @examples
#' data_table <- data.frame(
#'   name = c("dirtydata", "meta"),
#'   file = c("dirtydata.csv", "meta_table.xlsx"),
#'   folder = system.file("extdata", package = "cctu"),
#'   stringsAsFactors = FALSE
#' )
#' data_table_summary(data_table)
#' options("verbose" = TRUE)
#' read_data(data_table)
#' summary(dirtydata)
#' summary(meta)
#'
#' @export
#' @seealso \code{\link{read.csv}} \code{\link[readxl]{read_excel}}
#' \code{\link{data_table_summary}}
#' \code{\link{apply_macro_dict}}
#' \code{\link{var_lab}}
#' \code{\link{extract_form}}


read_data <- function(x, ...) {
  UseMethod("read_data", x)
}

#' @describeIn read_data data.frame method for read_data generic
#' @export
read_data.data.frame <- function(x,
                                 name_variable = "name",
                                 file_variable = "file",
                                 ...) {
  grandparent <- parent.frame()
  dots <- list(...)

  if ("folder" %in% names(x)) {
    folder <- x[, "folder"]
    folder <- ifelse(is.na(folder) | folder == "", getwd(),
      normalizePath(folder)
    )
    x[, file_variable] <- file.path(folder, x[, file_variable])
  }

  for (obj in x[, name_variable]) {
    output <- do.call(read_data.character,
      c(
        list(
          x = obj,
          data_table = x,
          name_variable = name_variable,
          file_variable = file_variable
        ),
        dots
      ),
      envir = grandparent
    )
  }
  if (exists("frame", dots) && is.null(dots$frame)) {
    return(output)
  }
}

#' @describeIn read_data  character method for read_data generic
#' @export
read_data.character <- function(x,
                                data_table,
                                fun = NULL,
                                frame = parent.frame(),
                                name_variable = "name",
                                file_variable = "file",
                                clean_names_option = FALSE,
                                remove_blank_rows_cols_option = FALSE,
                                ...) {
  names <- data_table[, name_variable]
  files <- data_table[, file_variable]
  file <- as.character(files[match(x, names)])

  if (is.null(fun)) {
    fun <- utils::read.csv
    # some sort of intelligent setting of fun
    if (grepl("\\.xlsx?$", file)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package \"readxl\" needed for this function to load excel files")
      }
      fun <- readxl::read_excel
    }
  } else if (all(!grepl("read", deparse(substitute(fun))))) {
    warning(
      paste(
        "this function is designed to be used for reading in data. You are calling:",
        paste(deparse(substitute(fun)), collapse = ", ")
      )
    )
  }

  output <- do.call(fun, c(list(file), ... = ...))
  if (clean_names_option) {
    clean_names(output)
  }
  if (remove_blank_rows_cols_option) {
    remove_blank_rows_cols(output)
  }

  if (is.null(frame)) {
    return(output)
  } else {
    assign(x, output, envir = frame)
    if (options()$verbose) {
      cat(paste0("object created in ", environmentName(frame), ": ", x, "\n"))
    }
  }
}
