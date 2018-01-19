#' Function to write a table into xml format in the correct directory, and edit TableofTables
#'
#' @inheritParams write_ggplot
#' @param X the data.frame or table to be saved in xml format
#' @param heading character vector of column titles. Defaults to the colnames of X
#'
#' @return writes an xml version of the input data to file table_number.xml . Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{get_file_name}} \code{\link{write_ggplot}}


write_table = function(
                      number,
                      X,
                      heading  = colnames(X),
                      clean_up = TRUE,
                      directory="/Output/Figures",
                      path_string="PATH",
                      ...
                      ){


  PATH <- get_obj(path_string, alt=getwd())
  #
  if(is.null(sys.calls())){
    CallingProg <- "Missing"
  } else {
    CallingProg <- get_file_name()
  }



  add_program(number, CallingProg, ... )


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
      text = as.character(X[row, col])
      text = gsub("&","&amp;", text)
      text = gsub("<","&lt;", text)
      text = gsub(">", "&gt;", text)
      paste_plus("<td>", text, "</td>")

    }
    paste_plus("</tr>\n")
  }
  paste_plus("</table>\n")

  file_name <- paste0(PATH, directory,"/table_",number,".xml")

  cat(output_string, file = file_name, append = FALSE)





  if(clean_up){
    parent_frame <- parent.frame()
    clean_up(number, envir = parent_frame,...)
  }
}
