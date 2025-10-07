#' sorts inputs that look like Dewey decimal numbering
#' @param x a vector of character strings
#'
#' @return  a numeric vector that sorts the input as per dewey decimal system,
#' without confusing 10.2, vs 10.10
#' @export



order_dewey <- function(x) {
  if (!inherits(x, "character")) {
    stop("Must input a character vector")
  }
  # split up the string into a list of vectors broken by decimals
  xx <- strsplit(x, "\\.")
  check_non_numeric <- sapply(xx, function(x) {
    any(grepl("\\D", x))
  })
  if (any(check_non_numeric)) {
    stop("non numeric values are not allowed")
  }
  # FInd out the longest length
  nrow <- max(sapply(xx, length))
  # Pad the empty elements out and return a matrix
  xx <- sapply(xx, function(x) {
    c(as.integer(x), rep(NA, nrow - length(x)))
  })
  # a trick to get xx technically as a list of columns, and then allow
  # order() over an arbitrary number of columns as arguments to break ties
  if (nrow > 1) {
    xx <- as.data.frame(t(xx))
  } else {
    xx <- as.data.frame(xx)
  }
  do.call(order, xx)
}
