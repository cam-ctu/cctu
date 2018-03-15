#' removes all the objects in the environment, apart from those listed in reserved and detaches the environment used to create tables or figures corresponding to the number
#'
#' @param number a number associated with a table of figure
#' @param envir an environment to clean up
#' @param reserved_string a character giving the name of a global variable that names gloabl objects that should generally be preserved globally when tidying up.
#' @inheritParams detach_pop
#' @param ... modifications to the default values of meta_table_string, reserved_string, popn_table_string see \code{\link{clean_up}}
#'
#' @return NULL, but removes objects
#' @export

clean_up <- function(number,
                     envir = parent.frame(),
                     reserved_string="RESERVED",
                     ...
                     ){
  obj_list <- ls(envir)
  RESERVED <- get_obj(reserved_string, frame=envir)
  keep     <- match(c(reserved_string, RESERVED), obj_list, nomatch=0)
  obj_list <- obj_list[-keep]
  rm(list = obj_list, envir = envir)
  detach_pop(number, frame=envir, ...)
}
