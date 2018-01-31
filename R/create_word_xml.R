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
#'
#' @details  all file paths need to use "\\" as "/" will not work in windows dos




create_word_xml <- function(
  report_title,
  author,
  filename="Output\\Reports\\Report.doc",
  path_string="PATH",
  meta_table_string="meta_table",
  table_path="Output\\Core\\",
  figure_format=c("png","jpeg","ps"),
  figure_path="Output\\Figures\\"
){
  meta_table <- get_obj(meta_table_string) %>%
    subset( !is.na(Number))
  ## CHekcs don't like this, but are OK using with(.)
    #meta_table = meta_table[!is.na(meta_table$Number), ]


  #check you are in the right working directory
  PATH <- get_obj(path_string, alt=getwd())
  if(getwd()!=sub("/$","",PATH)){warning(paste("you are calling create_word_xml with the working directory not equal to", PATH))}

  ### NEED to put the header into the package data somehow and call it

  # put the file in /inst/extdata
  # use system.file("extdata", "header.txt", package="cctu")

  call     = paste0('copy "',
                    system.file("extdata", "header.txt", package="cctu") %>% gsub("/","\\\\",.),
                    #Output\\Reports\\header.txt"
                    '" "' ,
                    filename, '" /Y')
  shell(call)

  #put a copy of the xslt file into the report path
  report_folder <- normalizePath(paste0(PATH,filename,"/.."))
  call = paste0('copy "',
                system.file("extdata", "xml_to_word.xslt", package="cctu") %>% gsub("/","\\\\",.),
                '"  "',report_folder, '" /Y'
                )
  shell(call)

  cat(
    "\n <Report>
  <study>",report_title,"</study>
  <author>",author,"</author><datestamp>",
    date(), "</datestamp>", file = filename, append = TRUE)

  ## not sure this will wash in a package as loads of these variabls are not defined

  #need to write a function that cheks if meta_table has the right set of varaible/names

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
  figure_format <- match.arg(figure_format)

    for(i in 1:length(headers)){
    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "Item"] == "Table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      call = paste0('type "',table_path,'table_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Figure"){
      cat("\n <MetaFigure> \n", headers[i], file = filename, append = TRUE)
      cat("<src>", PATH, figure_path,"fig_", meta_table[i, "Number"],
          ".",figure_format,"</src>", sep = "", file = filename, append = TRUE)
      cat(footers[i], Program[i], "\n </MetaFigure> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      call = paste0('type "',table_path,'text_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  cat("\n </Report>", file = filename, append = TRUE)

}
