#' Functions that attach and detach functions based on which populations are used for a given table number
#'
#' @param number the number of a table or figure
#' @param meta_table_string character string of the name of a global table that contains meta information on tables (title, population, number)
#' @param popn_table_string character string of the name of a gloabl table that contains population names used in meta-table, and the equivalent names of environments
#' @param ... modifications to the default values of meta_table_string, reserved_string, popn_table_string see \code{\link{clean_up}}
#'
#' @return invisibly returns an environment for attaching, or NULL for detaching.
#' @seealso \code{\link{attach}} \code{\link{detach}}
#' @importFrom magrittr %>%





#'@describeIn attach_pop attaches a population
#'@export
attach_pop <- function(number,
                       ...
                       ){
  .eval_pop(number, function_name="attach", frame=parent.frame(),
            ...)
}

#' @describeIn attach_pop detaches a populations
#' @export

detach_pop <- function(number,
                       ... ){
  .eval_pop(number, function_name="detach", frame=parent.frame(),
            ...)
}

#'@describeIn attach_pop internal function
#'@keywords internal

.eval_pop <- function(number,
                     function_name,
                     meta_table_string="meta_table",
                     popn_table_string="popn_table",
                     frame=parent.frame(),
                     ...
                     # needed to cope with functions who want PATH="C:" say
){
  #print(frame)
  meta_table <- get_obj(meta_table_string, frame=frame)
  popn_table <- get_obj(popn_table_string, frame=frame)
  meta_table <- merge(meta_table,popn_table,
                      by.x = "Population", by.y = "title", all.x = TRUE)
  index <- match(number, meta_table$Number)
  pop_name <- meta_table[index, "pop_name"] %>% as.character
  eval(call(function_name, as.name(pop_name)), envir=frame)
}
