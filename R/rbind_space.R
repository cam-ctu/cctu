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
  if(ncol(x) != ncol(y)){stop("the number of columns do not match")}
  blank_row <- rep("", ncol(x))
  rbind(x, blank_row, y, deparse.level = 0)
}
