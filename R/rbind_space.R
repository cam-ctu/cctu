#### Function to bind tables/matrices with a blank row between them
PROGNAME <- "rbind_space.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 
#### Notes: 
#### - use with Reduce for a list of tables


rbind_space <- function(x, y){
  if(ncol(x) != ncol(y)){stop("the number of columns do not match")}
  blank_row <- rep("", ncol(x))
  rbind(x, blank_row, y, deparse.level = 0)
}
