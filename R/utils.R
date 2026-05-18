#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Check if All Elements in Character Vector are Numeric
#'
#' Tests, without issuing warnings, whether all elements of a character
#' vector are legal numeric values, or optionally converts the vector to
#'  a numeric vector. Leading and trailing blanks in x are ignored.
#'
#' @param x a character vector
#' @param extras a vector of character strings to count as numeric values,
#' other than "".
#'
#' @return a logical value
#'
#' @examples
#' all_is_numeric(c("1", "1.2", "3"))
#' all_is_numeric(c("1", "1.2", "3a"))
#'
#' @export
all_is_numeric <- function(x, extras = c(".", NA)) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  xs <- x[!x %in% c("", extras)]
  if (!length(xs)) {
    return(FALSE)
  }
  suppressWarnings(!any(is.na(as.numeric(xs))))
}

