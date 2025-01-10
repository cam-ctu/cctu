#' Function to write a text into xml format in the correct directory, and edit TableofTables
#'
#' @inheritParams write_ggplot
#' @param X the character string to be saved in xml format

#'
#' @return writes an xml version of the input data to file text_number.xml . Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{write_table}}
#' @importFrom magrittr %>% %<>%

write_text <- function(X,
                       number=cctu_env$number,
                       clean_up = TRUE,
                       directory=file.path(getOption("cctu_output", default = "Output"),"Core"),
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

  paste_plus("<text>\n")
  X <- remove_xml_specials(X)
  paste_plus(X)
  paste_plus("</text>\n")

  file_name <- file.path(directory,paste0("text_",number,".xml"))

  cat(output_string, file = file_name, append = FALSE)
  if(verbose){cat("\n", file_name, "created.\n")}




  if(clean_up){
    clean_up(number, frame = parent.frame(), verbose=verbose)
  }
}
