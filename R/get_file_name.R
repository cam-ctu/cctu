#' Function to get the filename and path of the most deeply nested call to source(), or whatever function in pattern
#'
#' @param pattern the name of a function to search for
#' @param ... arguments to supply to \code{\link{normalizePath}}
#'
#' @return a character string that is the value of \code{\link{normalizePath}} applied to the first
#' argument of the most deeply nested call to \code{pattern}. Or, in lay terms, if you use source()
#' in a nested fashion, then calling get_file_name() will give the name of the file it is called within.
#'
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{write_table}}

get_file_name <- function(pattern="source",...){
  calls <- sys.calls()
  if(!is.null(calls)){
    commands <- sapply(calls,function(x){as.character(x)[1]})
    index <- grep(pattern,commands)
    if(length(index)){
      normalizePath(as.character(calls[[max(index)]])[2], ...)
    } else{
      NULL
    }
  } else{
    NULL
  }
}


#'@describeIn get_file_name finds the file that was sourced to call the current file
#'@export
#'
#'@param root a character string of the root file to default to when recording the value of parent file. This cannot be found automatically if it is run iteractively on the command line.
#'
#'@return \code{\link{get_parent}} is similar to \code{\link{get_file_name}} but is used _inside_  the cctu package function \code{\link[cctu]{source}},
#'so we need to look one level up the sequence of calls to get the filename that calls source().
#'@seealso \code{\link[cctu]{source}}
#'@export

get_parent <- function(pattern="source",root="Main.R", ...){
  calls <- sys.calls()
  if (!is.null(calls)) {
    commands <- sapply(calls, function(x) {
      as.character(x)[1]
    })
    index <- grep(pattern, commands)
    if (length(index)>1) {
      index <- sort(index, decreasing = TRUE)
      normalizePath(as.character(calls[[index[2]]])[2],
                    ...)
    }
    else {
      normalizePath(root)
    }
  }
  else {
    normalizePath("Main.R")
  }

}
