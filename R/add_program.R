#' program to add details of a code file path to the meta-table
#'
#' @param number the number of the table or figure
#' @param calling_prog the file path
#' @inheritParams clean_up
#'
#' @keywords internal




add_program <- function(number, calling_prog, meta_table_string="meta_table", frame=parent.frame(), ...){
  if( exists(meta_table_string, where=frame)){
    meta_table <- get(meta_table_string, pos=frame)
    if( !("Number" %in% names(meta_table))){warning(paste("Need to have Number column in", meta_table_string))}
    if( !("Program" %in% names(meta_table))){warning(paste("Need to have Program column in", meta_table_string))}
    index <- match(number, meta_table$Number)
    meta_table[index, "Program"] <- calling_prog
    assign(meta_table_string,meta_table, envir = where(meta_table_string, env=frame))
  } else{
    warning(paste("''",meta_table_string,"'' was not defined in the parent frame"))
  }
}
