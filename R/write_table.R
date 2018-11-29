#' Function to write a table into xml format in the correct directory, and edit TableofTables
#'
#' @inheritParams write_ggplot
#' @param X the data.frame or table to be saved in xml format
#' @param heading character vector of column titles. Defaults to the colnames of X
#' @param na_to_empty logical, if true then any NA values will be written as empty strings. Defaults to false.
#'
#' @return writes an xml version of the input data to file table_number.xml . Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{get_file_name}} \code{\link{write_ggplot}}
#' @importFrom magrittr %>% %<>%

write_table = function(X,
                      number=cctu_env$number,
                      heading  = colnames(X),
                      na_to_empty=FALSE,
                      clean_up = TRUE,
                      directory="Output\\Core\\",
                      frame=parent.frame(),
                      ...
                      ){

  # don't need this unless want to override
  #PATH <- get_obj(path_string, frame=frame, alt=getwd())
  #

  CallingProg <- get_file_name()
  if(is.null(CallingProg)){
    warning(paste("Unable to identify the code file that created table", number))
    CallingProg <- "Missing"
    }
  add_program(number, CallingProg)


  output_string <- NULL

  paste_plus <- function(..., initial = output_string){
    arg_name <- deparse(substitute(initial))
    initial  <- paste(initial, ...)
    assign(arg_name, initial, envir = parent.frame())
  }

  paste_plus("<table>\n<tr>")
  for(col in 1:dim(X)[2]){
    paste_plus("<td>", heading[col], "</td>")
  }
  paste_plus("</tr>\n")
  for(row in 1:dim(X)[1]){
    paste_plus("<tr>")
    for(col in 1:dim(X)[2]){
      text <-  as.character(X[row, col])
      if(na_to_empty){  text <- ifelse(is.na(text),"", text) }
      text <- remove_xml_specials(text)
      paste_plus("<td>", text, "</td>")

    }
    paste_plus("</tr>\n")
  }
  paste_plus("</table>\n")


  directory %<>% normalizePath %>% final_slash
  file_name <- paste0(directory,"table_",number,".xml")

  cat(output_string, file = file_name, append = FALSE)





  if(clean_up){
     clean_up(number, frame = frame,...)
  }
}


#' @keywords internal
#'

remove_xml_specials <- function(x){
  x <- gsub("&(?!#\\d+;)","&amp;\\1", x,perl=TRUE)
  x <-  gsub("<","&lt;", x)
  x <-  gsub(">", "&gt;",x)
  x <- gsub('"', "&quot;",x)
  x <- gsub("'", "&apos;",x)
  x
}





