#### Wrapper function to save typing/avoid errors
PROGNAME <- "srce.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 
#### Notes: 
#### - have to apply the chdir = TRUE argument to record the correct file path when
####   printing the code file in the footnote of the output


srce <- function(file, backup = FALSE){
  # to allow you to rewind and re-run code
  if(backup){save.image(file = paste0(PATH, "\\Output\\R_images\\backup.Rdata"))}
  # use load(file = paste0(PATH, "/Output/R_images/backup.Rdata"))
  # before re-running if needed.
  source(paste0("Progs\\Analysis\\", file), chdir = TRUE, echo = TRUE, max.deparse.length = 1500000)
}