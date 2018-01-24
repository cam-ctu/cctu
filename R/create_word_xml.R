#' Produces the final xml file
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param filename text string giving the filename/path to output the word document to
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables
#' @param figure_path text string giving the filepath to the figures
#'
#' @export
#' @importFrom magrittr %>%




create_word_xml <- function(
  report_title,
  author,
  filename="Output/Reports/Report.xml",
  path_string="PATH",
  meta_table_string="meta_table",
  table_path="Output/Core/",
  figure_path="Output/Figures/"
){
  meta_table <- get_obj(meta_table_string) %>%
    subset( !is.na(Number))
  ## CHekcs don't like this, but are OK using with(.)
    #meta_table = meta_table[!is.na(meta_table$Number), ]


  #check you are in the right working directory
  PATH <- get_obj(path_string, alt=getwd())
  if(getwd()!=PATH){warning(paste("you are calling create_word_xml with the working directory not equal to", PATH))}

  ### NEED to put the header into the package data somehow and call it

  # put the file in /inst/extdata
  # use system.file("extdata", "header.txt", package="cctu")

  call     = paste0('copy "',
                    system.file("extdata", "header.txt", package="cctu"),
                    #Output\\Reports\\header.txt"
                    '" "' ,
                    filename, '" /Y')
  shell(call)
  cat(
    "\n <Report>
  <study>",report_title,"</study>
  <author>",author,"</author><datestamp>",
    date(), "</datestamp>", file = filename, append = TRUE)

  ## not sure this will wash in a package as loads of these variabls are not defined

  headers = with(meta_table,
                 paste0("<heading><section>", Section,
                        "</section><title>", Title,
                        "</title><population>", Population,
                        "</population><subtitle>",
                        ifelse(is.na(Subtitle), "N/A", as.character(Subtitle)),
                        "</subtitle><number>", Number,
                        "</number><orientation>", Orientation,
                        "</orientation></heading>"))

  footers = with(meta_table,
                 paste("<footnote>",
                       ifelse(is.na(Footnote1), "", Footnote1),
                       "</footnote><footnote>",
                       ifelse(is.na(Footnote2), "", Footnote2),
                       "</footnote>"))

  Program = with(meta_table, paste("<Program>", Program, "</Program>"))


  ### CONVERT the hardcoded filepaths into the function arguments

    for(i in 1:length(headers)){
    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "Item"] == "Table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      call = paste0('type "Output\\Core\\table_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Figure"){
      cat("\n <MetaFigure> \n", headers[i], file = filename, append = TRUE)
      cat("<src>", PATH, "\\Output\\Figures\\fig_", meta_table[i, "Number"],
          ".png</src>", sep = "", file = filename, append = TRUE)
      cat(footers[i], Program[i], "\n </MetaFigure> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      call = paste0('type "Output\\Core\\text_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  cat("\n </Report>", file = filename, append = TRUE)

}
