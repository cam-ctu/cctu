
#' Combine descriptive statistics table by rows
#'
#' @param ... \code{stat_tab} descriptive statistics table .
#'
#' @export
#'
rbind.cttab <- function(...){
  allargs <- list(...)
  allargs <- Filter(Negate(is.null), allargs)

  all_class <- sapply(allargs, function(x)inherits(x, "cttab"))
  if(any(!all_class))
    stop("Only cttab class is supportted.")

  pos <- unlist(lapply(allargs, function(x)attr(x, "position")))
  obj_class <- unique(unlist(lapply(allargs, class)))

  # Remove cttab class for rbind
  allargs <- lapply(allargs, function(x){
    l <- which(attr(x, "class") == "cttab")
    attr(x, "class") <- attr(x, "class")[-l]
    return(x)
  })

  structure(do.call(base::rbind, allargs),
            position = pos,
            class = obj_class)

}


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
#' all_is_numeric(c('1','1.2','3'))
#' all_is_numeric(c('1','1.2','3a'))
#'
#' @export


all_is_numeric <- function(x, extras=c('.','NA')){
  x <- sub('[[:space:]]+$', '', x)
  x <- sub('^[[:space:]]+', '', x)
  xs <- x[!x %in% c('',extras)]
  if(!length(xs))
    FALSE
  suppressWarnings(!any(is.na(as.numeric(xs))))
}
