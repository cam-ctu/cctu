#' Function to write a text into xml format in the correct directory, and edit
#' TableofTables
#'
#' @param x the character string (or vector of strings) to be saved in xml
#'   format. A vector is concatenated into a single text block.
#' @param number the number used as a suffix in the output filename, and to
#'   link to the meta_table. Default is to use the value in the cctu_env
#'   package environment that is set within \code{\link{attach_pop}}.
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at
#'   the end. Defaults to \code{TRUE}.
#' @param directory where to save the text within path or current working
#'   directory. Defaults to \code{file.path(cctu_opt("output"), "Core")}.
#' @param verbose logical to print information on changes to the global
#'   environment or external files. Defaults to \code{options()$verbose}.
#'
#' @return writes an xml version of the input data to file text_number.xml .
#' Edits the TableofTables object with the calling programe. No return object.
#' @export
#' @seealso \code{\link{write_ggplot}} \code{\link{write_table}}
#' @importFrom magrittr %>% %<>%

write_text <- function(x,
                       number = cctu_env$number,
                       clean_up = TRUE,
                       directory = file.path(cctu_opt("output"), "Core"),
                       verbose = options()$verbose) {
  calling_prog <- cctu_env$parent[1] # get_file_name()
  if (is.null(calling_prog)) {
    warning(paste(
      "Unable to identify the code file that created text",
      number
    ))
    calling_prog <- "Missing"
  }
  add_program(number, calling_prog)


  x <- remove_xml_specials(x)
  output_string <- paste("<text>\n", x, "</text>\n")

  file_name <- file.path(directory, paste0("text_", number, ".xml"))

  cat(output_string, file = file_name, append = FALSE)
  if (verbose) {
    cat("\n", file_name, "created.\n")
  }

  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
}
