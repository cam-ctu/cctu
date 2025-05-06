#' loads all the packages listed in the DESCRIPTION file
#'
#' @returns a list of the packages found in DESCRIPTION
#'
#' @importFrom desc desc_get_deps
#' @export
#'

library_description <- function(){
  if (!file.exists("DESCRIPTION")) {
    warning("No DESCRIPTION file so no packages loaded")
  } else {
    lapply( desc_get_deps()$package ,
       library, character.only = TRUE
      )
    desc_get_deps()$package
  }
}
