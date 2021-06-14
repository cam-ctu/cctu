#' Function to write a table into xml format in the correct directory, and edit TableofTables
#'
#' @inheritParams write_ggplot
#' @param X the data.frame or table to be saved in xml format
#' @param heading character vector of column titles. Defaults to the colnames of X
#' @param na_to_empty logical, if true then any NA values will be written as empty strings. Defaults to false.
#'
#' @return writes an xml version of the input data to file table_number.xml . Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{detect_invalid_utf8}} \code{\link{remove_invalid_utf8}}
#' @importFrom magrittr %>% %<>%

write_table = function(X,
                      number=cctu_env$number,
                      heading  = colnames(X),
                      na_to_empty=FALSE,
                      clean_up = TRUE,
                      directory=file.path("Output","Core"),
                      verbose=options()$verbose
                      ){



  CallingProg <- cctu_env$parent[1]  #get_file_name()
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
  if( inherits(X,c("tbl_df","matrix","array"))){X <- as.data.frame(X)}
  utf8_check <- detect_invalid_utf8(X)
  if( nrow(utf8_check)){
    warning("Invalid non-UTF8 characters found\n", utf8_check,
            "\nRemove manually from the xml output, or clean input data at source,
   or clean using remove__invalid_utf8()")
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


  #directory %<>% normalizePath %>% final_slash
  file_name <- file.path(directory,paste0("table_",number,".xml"))

  cat(output_string, file = file_name, append = FALSE)
  if(verbose){cat("\n", file_name, "created.\n")}




  if(clean_up){
     clean_up(number, frame = parent.frame(), verbose=verbose)
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





