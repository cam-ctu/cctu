#' Function to write to meta-table and clean up without creating a table,text,
#' or figure. For manual use
#'
#' @inheritParams write_ggplot
#' @return Edits the meta_table object with the calling program and optionally
#' cleans up. No return object.  Meant for use if a table or figure for a
#' report has been created manually outside of \code{\link{write_ggplot}} or
#'  \code{\link{write_table}}
#' @export
#' @seealso \code{\link{write_ggplot}}  \code{\link{write_table}}


# NOT YET INCLUDED IN TESTING


write_manual <- function(number = cctu_env$number,
                         clean_up = TRUE,
                         verbose = options()$verbose) {
  calling_prog <- cctu_env$parent[1] # get_file_name()
  if (is.null(calling_prog)) {
    warning(paste(
      "Unable to identify the code file that created table",
      number
    ))
    calling_prog <- "Missing"
  }
  add_program(number, calling_prog)


  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
}
