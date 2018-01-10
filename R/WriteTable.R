#### Function to take df and write in xml format  
PROGNAME <- "WriteTable.R"
#### Author: Simon Bond
#### Study: ReACt
#### Input: A table, an index number, optionally heading text.
#### Output: an xml file in the OUtput\Core folder
#### DMC report Sep 2017
#### Date created: 11NOV2014
#### Notes: 
#### - 14/06/17 removed requirement for XML package


WriteTable = function(X,
                      number,
                      file     = paste0(PATH, "\\Output\\Core\\table_", number, ".xml"),
                      heading  = colnames(X),
                      clean_up = TRUE
                      ){

  # PATH and PROGNAME  need to be defined in the environment that calls this function
  if(is.null(PATH)){PATH = getwd()}
  
  #
  if(is.null(sys.calls())){
    CallingProg <- "Missing"
  } else {
    CallingProg <- get_file_name()
  }
  
  #if(is.null(PROGNAME)){PROGNAME = "Program missing"}
  
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
  
  cat(output_string, file = file, append = FALSE)

  #CallingProg = paste(getwd(), PROGNAME, sep = "\\")
  TableofTables[!is.na(TableofTables$Number) & as.character(TableofTables$Number) == number, "Program"] <- CallingProg
  assign("TableofTables", TableofTables, envir = .GlobalEnv)

  if(clean_up){
    parent_frame <- parent.frame()
    clean_up(number, envir = parent_frame)
  }
}