#' A function to remove all attached objects from the search path with any exceptions specified
#'
#' @param ignore a character vector of regular expression search terms  within the output from \code{search()} as to which should be retained.
#'
#' @details it detaches anything not ignored.
#' @seealso  \code{\link{search}}
#' @export



rm_envir <- function(ignore=c(".GlobalEnv","package:","tools:","Autoloads")){
  searchpath <- search()
  for( pattern in ignore){
    searchpath <-  searchpath[ !grepl(pattern, searchpath) ]
  }
  for( item in searchpath){
      detach(item, character.only = TRUE)
  }
}
