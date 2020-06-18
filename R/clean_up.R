#' removes all the objects in the environment, apart from those listed in reserved and detaches the environment used to create tables or figures corresponding to the number
#'
#' @param number a number associated with a table of figure
#' @param frame an environment in which to clean up
#' @param reserved_string a character giving the name of a global variable that names gloabl objects that should generally be preserved globally when tidying up.
#' @inheritParams detach_pop
#'
#' @return NULL, but removes objects
#' @export

clean_up <- function(number,
                     frame = parent.frame(),
                     reserved_string=".reserved",
                     verbose=options()$verbose
                     ){
  RESERVED <- get_obj(reserved_string, frame=frame)
  if(is.null(RESERVED)){
    warning(
      paste0("'",reserved_string,
             "' does not exist. All objects in ",
             format(frame)," will be removed by clean_up().")
    )}
  obj_list <- ls(frame, all.names = FALSE)
  keep     <- match(RESERVED, obj_list, nomatch=0)
  if(length(keep)>1 || ( length(keep==1) & all(keep!=0) ) ){obj_list <- obj_list[-keep]}
  rm(list = obj_list, envir = frame)
  if(verbose){cat("\nObjects removed:", obj_list,"\n")}
  detach_pop(number,verbose=verbose)
  cctu_env$number <- "0"
  cctu_env$sumby_count <- 0
}
