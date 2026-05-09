#' Function to write to meta-table and clean up without creating a table,text,
#' or figure. For manual use
#'
#' @param number the number used to link to the meta_table. Default is to use
#'   the value in the cctu_env package environment that is set within
#'   \code{\link{attach_pop}}.
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at
#'   the end. Defaults to \code{TRUE}.
#' @param verbose logical to print information on changes to the global
#'   environment or external files. Defaults to \code{options()$verbose}.
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
