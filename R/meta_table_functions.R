#'Functions to handle meta_table
#'
#' @param meta_table a data.frame to be  set as the meta_table internal object
#' @return get_table returns the global object, set_table invisibly returns the previous version

#' @describeIn get_meta_table  gets a copy of the internal object
#' @export
get_meta_table <- function(){
  cctu_env$meta_table
}

#' @describeIn get_meta_table sets the internal object to the argument provided, whilst carrying out some cleaning
#' @export
set_meta_table <- function(meta_table){
  old <- cctu_env$meta_table
  meta_table <-clean_meta_table(meta_table)
  cctu_env$meta_table <- meta_table
  invisible(old)
}


#' @param number the number of the table or figure
#' @param calling_prog the file path
#' @inheritParams clean_up
#' @inheritParams attach_pop
#'
#' @keywords internal


add_program <- function(number, calling_prog){

  meta_table <- get_meta_table()
  if( !("number" %in% names(meta_table))){warning("Need to have 'number' column in meta_table")}
  if( !("program" %in% names(meta_table))){warning("Need to have 'program' column in meta_table")}
  index <- match(number, meta_table$number)
  meta_table[index, "program"] <- calling_prog
  set_meta_table(meta_table)
}





#' @keywords internal
#'

clean_meta_table <- function(meta_table){
  op <- options()
  options(stringsAsFactors = FALSE)
  columns_needed <- c("section","title","subtitle","number","population",
                      "orientation", "program", "item", "footnote1","footnote2")



  if( !("number" %in% names(meta_table))){warning("Need to have 'number' column in meta_table")}
  if( !("item" %in% names(meta_table))){warning("Need to have 'item' column meta_table")}

  meta_table$number <- gsub("\\s","", meta_table$number)
  pmat <- pmatch(names(meta_table), columns_needed)
  meta_table <- subset( meta_table, !is.na(meta_table$number) & meta_table$number!="", select=!is.na(pmat))
  pmat <- pmatch(names(meta_table), columns_needed)
  names(meta_table) <- columns_needed[pmat]
  index <- meta_table$number %>% as.character %>% order_dewey
  meta_table <- meta_table[index,]
  n <- nrow(meta_table)

  extra_cols <- columns_needed[-pmat]
  if(length(extra_cols)){
    X <- matrix(" ", nrow=n, ncol=length(extra_cols))
    X <- as.data.frame(X)
    names(X) <- extra_cols
    meta_table <- cbind(meta_table, X)
  }

  meta_table$population <- gsub("\\s","", meta_table$population)

  if( "orientation" %in% extra_cols){
    meta_table$orientation <- "portrait"
  }

  item_index <- pmatch(meta_table$item %>% tolower,
                       c("table","figure","text"),
                       duplicates.ok = TRUE)
  if(any(is.na(item_index))){
    warning("invalid values for 'item' converted to 'table'")
    item_index <- ifelse(is.na(item_index),1, item_index)
  }
  meta_table$item <- c("table","figure","text")[item_index]

  orient_index <- pmatch(meta_table$orientation %>% tolower,
                         c("portrait", "landscape"),
                         duplicates.ok=TRUE)
  if(any(is.na(orient_index))){
    warning("invalid values for 'orientation' converted to 'portrait'")
    orient_index <- ifelse(is.na(orient_index), 1, orient_index)
  }
  meta_table$orientation <- c("portrait", "landscape")[orient_index]
  options(op)
  meta_table
}


