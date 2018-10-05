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
  meta_table <- get_obj(meta_table_string, frame=frame)
  index <- match(number, meta_table$Number)
  popn_name <- meta_table[index, "Population"] %>% as.character
  eval(call(function_name, as.name(popn_name)), envir=frame)
}



