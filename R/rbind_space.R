#' Function to bind tables/matrices with a blank row between them
#'
#' @param x a data.frame or array, possibly of character elements only
#' @param y a data.frame or array, with the same number of columns as \code{x}
#' @param check.names logical. If FALSE the names are preserved from \code{x}, but if TRUE they
#' are converted to syntactically valid variable names, which may not be desired for a table in a report.
#'
#' @return a data.frame or array that is \code{x} stacked vertically above \code{y} with a row
#' of blank values in-between. Useful for formatting
#'
#' @details based largely on the \code{\link{rbind}} function.
#' Use in conjunction with \code{\link{Reduce}} to join up more than two tables.
#'
#' @export


rbind_space <- function(x, y, check.names=FALSE){
  x <- tidy_input(x, check.names=check.names)
  y <- tidy_input(y, check.names=check.names)
  if(ncol(x) != ncol(y)){stop("the number of columns do not match")}
  blank_row <- rep("", ncol(x))
  rbind(x, blank_row, y, deparse.level = 0)
}

#' @keywords internal

tidy_input <- function(x, check.names=FALSE){
  if(!inherits(x, c("integer","factor","numeric","character","data.frame","matrix"))){stop("Invalid input class")}
  if(is.factor(x)){x <- as.character(x)}
  if(is.null(dim(x))){x <- t(x)}
  #if(is.character(x)|is.numeric(x) | is.integer(x)){x <- as.matrix(x)}
  if( is.data.frame(x)){  x <- as.data.frame(
    lapply(x, as.character),
    stringsAsFactors = FALSE,
    check.names=check.names
  )}
  x
}
