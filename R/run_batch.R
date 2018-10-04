#' Runs a script in batch mode directly from within R.
#'
#' This function is to allow R CMD BATCH to be called interactively, without needing to start
#' up a command line terminal and change directories etc.  It calls the R CMD BATCH from the current working directory.
#' Hence its value is in producing the side-effects: whatever happens when the script is run.
#' Plus a log file will be created in the working directory with the same name as the script but with the suffix changed to '.Rout'.
#'
#' The command will not run in environments where \code{\link{interactive}} is false, so as to prevent
#' infinite loops. Only expected to work in Windows.
#'
#'
#' @param filename character string of the name or path of the script (relative to the working directory).
#' @param ... other arguments to pass to \code{\link{shell}}
#'
#' @return  see \code{\link{shell}} which is called.
#'
#' @seealso \code{\link{BATCH}} and \code{\link[devtools]{RCMD}}, which might already do this....
#'
#' @seealso \code{\link[devtools]{check_failures}} for future ideas for a function to check the log file
#' for errors or warnings.





run_batch <- function(filename, ...){
  if( !interactive()){
    warning("run_batch() only works in interactive mode")
  } else {

    # belt and braces approach to use full filepaths with quotes.
    # It did work  with just "R CMD BATCH --vanilla filename", in a simple example.
    command <- paste0( R.home("bin"),
                       '/R.exe CMD BATCH --vanilla "',
                       normalizePath(filename),
                       '"')
    shell(command, ...=...)
  }
}
