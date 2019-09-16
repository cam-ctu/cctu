#' track which files of code are called by which oher files
#'
#' @return the data file containing two variables: parent ; child.
#'
#' @details each time the \code{cctu} function \code{\link{source}} is called,
#' an internal data frame is appended with the parent file that called \code{\link{source}},
#' and the child file that was sourced.
#'
#' You can reset the code_tree or examine it, but not directly edit it. It is reset automatically if
#' \code{\link{cctu_initialise}} is called.
#'
#' @seealso \code{\link{cctu_initialise}}
#' @export
get_code_tree <- function(){
  cctu_env$code_tree
}



#' @describeIn get_code_tree reset the internal code_tree object to have no entries
#' @param root_file the name of the code file you want to use as the initial root for code tree
#' @export
reset_code_tree <- function(root_file="main.R"){
  cctu_env$code_tree <- data.frame(parent = as.character(NULL),
                                   child = as.character(NULL),
                                   stringsAsFactors = FALSE)
  cctu_env$parent <- root_file

}
