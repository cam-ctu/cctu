
#' @title Check whether string, list or vector is empty
#' @name is_empty
#' @description This function checks whether a string or character vector,
#' a list or any vector (numeric, atomic) is empty or not.
#'
#' @param x String, character vector, list, data.frame or numeric vector or factor.
#' @param na_empty Logical, if \code{NA} is considered as empty (default is \code{TRUE}).
#' @return Logical vector.
#'
#' @examples
#' is_empty("test")
#' is_empty("")
#' is_empty(NA)
#' is_empty(NULL)
#'
#' # string is not empty
#' is_empty(" ")
#'
#' # however, this trimmed string is
#' is_empty(trimws(" "))
#'
#' # numeric vector
#' x <- 1
#' is_empty(x)
#' x <- x[-1]
#' is_empty(x)
#'
#' # check multiple elements of character vectors
#' is_empty(c("", "a"))
#'
#' # empty data frame
#' d <- data.frame()
#' is_empty(d)
#'
#' # empty list
#' is_empty(list(NULL))
#'
#' # NA vector
#' x <- rep(NA,5)
#' is_empty(x)
#' is_empty(x, na_empty = FALSE)
#' @export
is_empty <- function(x, na_empty = TRUE) {

  if(is.null(x))
    return(is.null(x))

  if(length(x) == 0)
    return(TRUE)

  sapply(x, function(val){

    stopifnot(is.atomic(val))

    # do we have a valid vector?
    if (!is.null(val)) {

      # if it's a character, check if we have only one element in that vector
      if (is.character(val)) {
        # characters may also be of length 0
        if (length(val) == 0 || (is.na(val) & na_empty))
          return(TRUE)

        # else, check all elements of x
        # but now R v4.3.1. nchar() does not work with invalid utf...
        val <- iconv(val, "UTF-8", "UTF-8",sub='*')
        zero_len <- nchar(val) == 0
        # return result for multiple elements of character vector
        return(unname(zero_len))
        # we have a non-character vector here. check for length
      } else {
        zero_len <- length(val) == 0
      }
    }

    is.null(val) || zero_len || (is.na(val) & na_empty)
  })

}
