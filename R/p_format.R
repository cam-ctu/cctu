#' formats p values with rounding and <0.001
#'
#' @param p vector of numeric p-values
#' @param digits the number of digits to round to, and define the threshold to
#' use the < symbol
#'
#' @return character vector of formated p-values
#'
#' @examples
#' p_format(c(0.000001, 0.451234))
#' @export


p_format <- function(p, digits = 3) {
  # check input
  if (!is.numeric(p)) {
    stop("'p_format' requires numeric input")
  }
  if (trunc(digits) != digits) {
    stop("'digits' argument must be an integer")
  }
  if (any(p < 0) || any(1 < p)) {
    warning("p-values outside of the [0,1] range")
  }
  if (digits < 1) {
    warning("ouput is rounding to the left of the decimal point")
  }
  # do the formatting
  threshold <- 10^(-digits)
  pchar <- round(p, digits = digits)
  fmt <- paste0("%1.", digits, "f")
  ifelse(p < threshold,
    paste0("<", format(threshold, scientific = FALSE)),
    paste0(" ", sprintf(fmt, pchar))
  )
}
