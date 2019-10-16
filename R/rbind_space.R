#' Function to bind tables/matrices with a blank row between them
#'
#' @param x a data.frame or array, possibly of character elements only
#' @param y a data.frame or array, with the same number of columns as \code{x}
#'
#' @return a data.frame or array that is \code{x} stacked vertically above \code{y} with a row
#' of blank values imbetween. Useful for formatting
#'
#' @details based largely on the \code{\link{rbind}} function.
#' Use in conjuction with \code{\link{Reduce}} to join up more than two tables.
#'
#' @export


rbind_space <- function(x, y){
  x <- tidy_input(x)
  y <- tidy_input(y)
  if(ncol(x) != ncol(y)){stop("the number of columns do not match")}
  blank_row <- rep("", ncol(x))
  rbind(x, blank_row, y, deparse.level = 0)
}

#' @keywords internal

tidy_input <- function(x){
  if(!inherits(x, c("integer","factor","numeric","character","data.frame","matrix"))){stop("Invalid input class")}
  if(is.factor(x)){x <- as.character(x)}
  if(is.vector(x)){x <- t(x)}
  if(is.character(x)|is.numeric(x) | is.integer(x)){x <- as.matrix(x)}
  if( is.data.frame(x)){  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)}
  x
}
