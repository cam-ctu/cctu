#' Produces the final xml file
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param meta_table a data frame  that contains meta information on tables (title, population, number)
#' @param datestamp text used to give the date-time stampe, defaults to the system date/time at the time of running the function
#' @param filename text string giving the filename/path to output the word document to
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables
#' @param figure_path text string giving the filepath to the figures
#' @param popn_labels alternative text string giving labels used for the population - might want to include the population size... They must match correctly to unique(meta_table$Population)
#'
#' @export
#' @importFrom magrittr %>% %<>%
#'
#' @details  all file paths need to use "\\" as "/" will not work in windows dos




create_word_xml <- function(
  report_title,
  author,
  meta_table,
  datestamp=format(Sys.time(),format="%H:%M %d %b %Y"),
  filename="Output\\Reports\\Report.doc",
  path=paste0(getwd(),"/"),
  table_path="Output\\Core\\",
  figure_format=c("png","jpeg","ps"),
  figure_path="Output\\Figures\\",
  frame=parent.frame(),
  popn_labels=NULL
){
  #PATH <- get_obj(path_string,frame=frame, alt="Missing Frame")
  #check you are in the right working directory
  if(getwd()!=sub("/$","",path)){warning(paste("you are calling create_word_xml with the working directory not equal to", path))}
  meta_table <- subset(meta_table,  !is.na(Number))
  index <- meta_table$Number %>% as.character %>% order_dewey
  meta_table <- meta_table[index,]

  ## CHekcs don't like this, but are OK using with(.)
    #meta_table = meta_table[!is.na(meta_table$Number), ]

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
  report_folder <- normalizePath(paste0(path,filename,"/.."))
  call = paste0('copy "',
                system.file("extdata", "xml_to_word.xslt", package="cctu") %>% gsub("/","\\\\",.),
                '"  "',report_folder, '" /Y'
                )
  shell(call)

  cat(
    "\n <Report>
  <study>",  remove_xml_specials(report_title),"</study>
  <author>",remove_xml_specials(author),"</author><datestamp>",
    datestamp, "</datestamp>", file = filename, append = TRUE)

  #need to write a function that cheks if meta_table has the right set of varaible/names

  #this makes the variable names in meta_table case insensitive
  if( any( names(meta_table) != names(meta_table) %>% propercase)){
    warning("the variable names in meta_table supplied have been converted to propercase")
    names(meta_table) %<>% propercase
  }

  if(!is.null(popn_labels)){
    index <- match(meta_table$Population, unique(meta_table$Population))
    meta_table$Population <- popn_labels[index]
  }


  headers = with(meta_table,
                 paste0("<heading><section>", Section,
                        "</section><title>", Title,
                        "</title><population>", Population,
                        "</population><subtitle>",
                        ifelse(is.na(Subtitle), "", remove_xml_specials(as.character(Subtitle))),
                        "</subtitle><number>", Number,
                        "</number><orientation>", Orientation,
                        "</orientation></heading>"))

  footers = with(meta_table,
                 paste("<footnote>",
                       ifelse(is.na(Footnote1), "", remove_xml_specials(as.character(Footnote1))),
                       "</footnote><footnote>",
                       ifelse(is.na(Footnote2), "", remove_xml_specials(as.character(Footnote2))),
                       "</footnote>"))

  Program = with(meta_table, paste("<Program>", Program, "</Program>"))
  figure_format <- match.arg(figure_format)

    for(i in 1:length(headers)){
    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "Item"] == "Table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      call = paste0('type "',normalizePath(table_path),'table_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Figure"){
      cat("\n <MetaFigure> \n", headers[i], file = filename, append = TRUE)
      cat("<src>", path, figure_path,"fig_", meta_table[i, "Number"],
          ".",figure_format,"</src>", sep = "", file = filename, append = TRUE)
      cat(footers[i], Program[i], "\n </MetaFigure> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "Item"] == "Text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      call = paste0('type "',normalizePath(table_path),'text_', meta_table[i, "Number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], Program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  cat("\n </Report>", file = filename, append = TRUE)

}
