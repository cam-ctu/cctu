#' Functions that attach and detach environments based on which populations are used for a given table number
#'
#' @param number the number of a table or figure
#' @param meta_table_string character string of the name of a global table that contains meta information on tables (title, population, number)
#' @param frame the environment or frame in which to attach or detach the desired target environment
#'
#'
#' @return invisibly returns an environment for attaching, or NULL for detaching.
#' @seealso \code{\link{attach}} \code{\link{detach}}
#' @importFrom magrittr %>%





#'@describeIn attach_pop attaches a population
#'@export
attach_pop <- function(number,
                       frame=parent.frame(),
                       meta_table_string="meta_table",...
                       ){
  .eval_pop("attach", number, frame, meta_table_string)
}

#' @describeIn attach_pop detaches a populations
#' @export

detach_pop <- function(number,
                       frame=parent.frame(),
                       meta_table_string="meta_table",...
                       ){
  .eval_pop("detach",number, frame, meta_table_string)
}

#' @keywords internal


.eval_pop <- function(function_name,number, frame, meta_table_string){
  if( exists(meta_table_string, where=frame)){
    meta_table <- get(meta_table_string, pos=frame)
    if( !("number" %in% names(meta_table))){warning(paste("Need to have 'number' column in", meta_table_string))}
    if( !("program" %in% names(meta_table))){warning(paste("Need to have 'program' column in", meta_table_string))}
    index <- match(number, meta_table$number)
    popn_name <- meta_table[index, "population"] %>% as.character
    if(popn_name %in% search() & function_name=="detach"){
      eval(call(function_name, as.name(popn_name)), envir=frame)
    }
    if( exists(popn_name, envir=frame) & function_name=="attach"){
      eval(call(function_name, as.name(popn_name)), envir=frame)
    }
  } else{
    warning(paste("''",meta_table_string,"'' was not defined in the parent frame"))
  }


}



