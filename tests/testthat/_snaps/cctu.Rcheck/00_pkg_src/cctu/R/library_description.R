#' loads all the packages listed in the DESCRIPTION file
#'
#' @returns a list of the packages found in DESCRIPTION
#'

#' @export
#'

library_description <- function(){
  if (!file.exists("DESCRIPTION")) {
    warning("No DESCRIPTION file so no packages loaded")
  } else {
    mylibs <- read.dcf("DESCRIPTION", fields="Imports") |>
      strsplit(",\n") |>
      unlist( use.names = FALSE)
    lapply( mylibs ,
       library, character.only = TRUE
      )
    mylibs
  }
}
