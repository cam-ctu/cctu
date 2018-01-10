#### Function to convert text to propercase?
PROGNAME <- "propercase.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 
#### Notes: 


propercase = function(x){
  gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", x, perl = TRUE)
}
