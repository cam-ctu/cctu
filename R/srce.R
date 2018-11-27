#'  Wrapper function for \code{\link{source}}
#'

#'
#'@inheritParams write_ggplot
#'@param file a file to source
#'@param backup a logical to indicate if you want to save an image prior to sourcing the course
#'@param backup_filepath a filepath to where to save the image
#'@param path_string text string giving the name of a variable that holds the path as a character string.
#'
#'@description Wrapper function for \code{\link{source}} to save typing/avoid errors, override some defaults, and save a backup copy
#'@export




srce <- function(file, backup = FALSE, path_string="PATH", backup_filepath= "Output\\R_images\\backup.Rdata"){
  # to allow you to rewind and re-run code
  PATH <- get_obj(path_string, alt=getwd())
  if(backup){save.image(file = paste0(PATH, backup_filepath))}
  # use load(file = paste0(PATH, "/Output/R_images/backup.Rdata"))
  # before re-running if needed.
  source(paste0("Progs\\Analysis\\", file),  local=FALSE)
}
