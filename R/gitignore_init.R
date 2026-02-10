#' checks to see if a .gitignore needs to be create or edited based on the template file
#'
#' @param template  the template .gitignore file.  Default is part of the package data.
#' @returns nothing, called for its side effect.  Gives messages out if  file is created or edited
#' @export

gitignore_init <- function(template = system.file("extdata/gitignore_template", package = "cctu")){
  # check for .gitignore file
  if ( !file.exists(".gitignore") ) {
    file.copy( from = template,
               to = ".gitignore"
               )
    message(".gitignore file created")
  } else {
    patterns <- readLines( template)
    file <- readLines(".gitignore")
    file_length <- length(file)
    for (pattern in patterns) {
      present <-  grepl( pattern, file, fixed = TRUE) |>  any()
      if (!present) file <- c(file, pattern)
    }
    if ( file_length < length(file)) {
      writeLines(file, ".gitignore")
      message(".gitignore edited")
    }
  }
}


