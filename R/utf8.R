#' Functions to detect and delete non-UTF8 characters, which the XML output
#' will not like
#'
#' @param data  a data.frame, typically the input to \code{write_table}
#'
#' @return \code{detect_invalid_utf8} returns a data.frame that gives the
#' column, row, and value of any datum points that contain invalid characters,
#' or a zero-length data frame if none are present.  \code{remove_invalid_utf8}
#' replaces any invalid characters with an empty string.
#'
#' @seealso \code{\link{write_table}}
#' @export

detect_invalid_utf8 <- function(data) {
  if (!inherits(data, "data.frame")) {
    stop("input a data.frame")
  }
  x <- lapply(data, invalid_utf8)
  where <- lapply(x, which)
  col_names <- names(where)
  cols <- rep(col_names, lapply(where, length))
  rows <- unlist(where)
  x_array <- array(unlist(x), dim = dim(data))
  # Tibble has issues, convert to data.frame
  data <- as.data.frame(data)
  data.frame(
    column = cols,
    row = rows,
    value = unlist(data[x_array]),
    row.names = NULL
  )
}

#' @describeIn detect_invalid_utf8 Function to delete invalid UTF8 characters
#' @export
remove_invalid_utf8 <- function(data) {
  if (!inherits(data, "data.frame")) {
    stop("input a data.frame")
  }
  x <- lapply(data, rm_invalid_utf8)
  data.frame(x)
}

#' @keywords internal
rm_invalid_utf8 <- function(x) {
  x <- iconv(x, "UTF-8", "UTF-8", sub = "")
  gsub("\u00A0", "\u0020", x)
}


#' @keywords internal

invalid_utf8 <- function(x) {
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")) |
    grepl("\u00A0", x)
}
