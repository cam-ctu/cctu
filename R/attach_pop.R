#' Functions that attach and detach environments based on which populations are used for a given table number
#'
#' @param number a character string, or number, giving the number of a table or figure. Be careful if you want '2.10' rather than 2.1, say .
#' @param verbose logical to print information on changes to the global environment or external files. Defaults to options()$verbose.
#' @param rm_envir logical, default=TRUE. Whether to run \code{\link{rm_envir}} first before attaching a population.
#'
#' @return invisibly returns an environment for attaching, or NULL for detaching.
#' @seealso \code{\link{attach}} \code{\link{detach}} \code{\link{rm_envir}}
#' @importFrom magrittr %>%



#'@describeIn attach_pop attaches a population
#'@export
attach_pop <- function(number,
                       verbose=options()$verbose,
                       rm_envir=TRUE
                       ){
  cctu_env$number <- as.character(number)
  cctu_env$sumby_count <- 0
  popn_name <- match_population(number)
  if(!is.null(popn_name) && popn_name !="" && exists(popn_name, where=parent.frame())){
    if(rm_envir){ rm_envir(verbose=verbose)}
    #attach is fussy about its argument needing to be an object, not a character
    eval(call("attach", as.name(popn_name)),envir=parent.frame())
    if(verbose){cat(popn_name,"attached containing:", ls(popn_name), "\n")}
  } else{
    warning("No population was attached")
  }
}


#' @describeIn attach_pop detaches a populations
#' @export

detach_pop <- function(number,
                       verbose=options()$verbose
                       ){
  popn_name <- match_population(number)
  if( !is.null(popn_name) && popn_name %in% search()){
    popn_list <- ls(popn_name)
    detach(popn_name, character.only = TRUE)
    if(verbose){cat(popn_name,"detached containing:", popn_list, "\n")}
  } else{
    warning("No population was detached")
  }
}


#' @keywords internal


match_population <- function(number){
    meta_table <- get_meta_table()
    if( !("number" %in% names(meta_table))){warning("Need to have 'number' column in meta_table")}
    if( !("population" %in% names(meta_table))){warning("Need to have 'population' column in meta_table")}
    index <- match(number, meta_table$number)
    popn_name <- meta_table[index, "population"] %>% as.character
    return(popn_name)
}




