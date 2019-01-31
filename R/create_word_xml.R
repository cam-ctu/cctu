#' Produces the final xml file
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param meta_table a data frame  that contains meta information on tables (title, population, number). Defaults is get_meta_table()
#' @param datestamp text used to give the date-time stampe, defaults to the system date/time at the time of running the function
#' @param filename text string giving the filename/path to output the word document to
#' @param path text string giving the parent folder in which the output file, table_path and figure_path are held. Defaults to the current working directory.
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables
#' @param figure_path text string giving the filepath to the figures
#' @param popn_labels alternative text string giving labels used for the population - might want to include the population size... They must match correctly to unique(meta_table$population), excluding rows with a blank, or no, population given
#' @param  figure_format the format to look for figure files when building the report ("png", "jpeg","ps")
#' @param xslt_file a text file containing the xslt document. Default is system.file("extdata", "xml_to_word.xslt", package="cctu").
#' @export
#' @importFrom magrittr %>% %<>%
#'
#' @return This function is run for its side-effects: creates an xml document that glues together all the outputs and meta data as per the meta-table argument; a transformation fo this as per the xslt file, the default can be opened as a word document.
#'
#' @details  all file paths need to use "\\" as "/" will not work in windows dos




create_word_xml <- function(
  report_title,
  author,
  meta_table=get_meta_table(),
  datestamp=format(Sys.time(),format="%H:%M %d %b %Y"),
  filename="Output\\Reports\\Report.doc",
  path=getwd(),
  table_path="Output\\Core\\",
  figure_format=c("png","jpeg","ps"),
  figure_path="Output\\Figures\\",
  popn_labels=NULL,
  verbose=options()$verbose,
  xslt_file=system.file("extdata", "xml_to_word.xslt", package="cctu")
){
  #check you are in the right working directory
  if(getwd()!=path){warning(paste("you are calling create_word_xml with the working directory not equal to", path))}
  #manage paths to deal with trailing slashes or not...
  path %<>% normalizePath %>% final_slash
  figure_path %<>% paste0(path, .) %>% normalizePath %>% final_slash
  table_path %<>% paste0(path, .) %>%normalizePath %>% final_slash
  long_filename <- paste0(path, filename) %>% normalizePath
  filename %<>% paste0(path, .) %>% normalizePath() %>% sub("\\.[^\\.]*$","", . , perl=TRUE) %>% paste0(.,".xml")

  meta_table <- clean_meta_table(meta_table)

  if(!is.null(popn_labels)){
    #preserve any non-population based tables.
    index <- match(meta_table$population, unique(c("",meta_table$population)))
    meta_table$population <- c("", popn_labels)[index]
  }



  ### NEED to put the header into the package data somehow and call it
  file.copy( system.file("extdata", "header.txt", package="cctu") ,
             filename, overwrite=TRUE
  )


  #put a copy of the xslt file into the report path
  # report_folder <- normalizePath(paste0(path,filename,"/.."))
  # file.copy(system.file("extdata", "xml_to_word.xslt", package="cctu"),
  #           report_folder, overwrite=TRUE
  # )

  cat(
    "\n <Report>
  <study>",  remove_xml_specials(report_title),"</study>
  <author>",remove_xml_specials(author),"</author><datestamp>",
    datestamp, "</datestamp>", file = filename, append = TRUE)

  headers = with(meta_table,
                 paste0("<heading><section>", section %>% as.character %>% remove_xml_specials,
                        "</section><title>", title %>% as.character %>% remove_xml_specials,
                        "</title><population>",
                        ifelse(is.na(population),"", remove_xml_specials(as.character(population))),
                        "</population><subtitle>",
                        ifelse(is.na(subtitle), "", remove_xml_specials(as.character(subtitle))),
                        "</subtitle><number>", number,
                        "</number><orientation>", orientation,
                        "</orientation></heading>"))

  footers = with(meta_table,
                 paste("<footnote>",
                       ifelse(is.na(footnote1), "", remove_xml_specials(as.character(footnote1))),
                       "</footnote><footnote>",
                       ifelse(is.na(footnote2), "", remove_xml_specials(as.character(footnote2))),
                       "</footnote>"))

  program =  paste("<Program>", meta_table$program, "</Program>")
  figure_format <- match.arg(figure_format)

  for(i in 1:length(headers)){
    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "item"] == "table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      file.append(filename, paste0( table_path,'table_', meta_table[i, "number"], '.xml'))
      cat(footers[i], program[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "item"] == "figure"){
      cat("\n <MetaFigure> \n", headers[i], file = filename, append = TRUE)
      cat("<src>", figure_path,"fig_", meta_table[i, "number"],
          ".",figure_format,"</src>", sep = "", file = filename, append = TRUE)
      cat(footers[i], program[i], "\n </MetaFigure> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "item"] == "text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      file.append(filename, paste0( table_path,'table_', meta_table[i, "number"], '.xml'))
      cat(footers[i], program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  cat("\n </Report>", file = filename, append = TRUE)

  #now apply the transform explicitly.
  doc <- xml2::read_xml(filename)
  transform <- xml2::read_xml(xslt_file)
  output <- xslt::xml_xslt(doc, transform)
  xml2::write_xml(output, file=long_filename)

  if(verbose){
    cat( long_filename, "created.",
      "\nAll figures in the word document are links to local files",
      "\nYou must manually include them with word if you want to move the word document.\n")
  }
}

#' @keywords internal
#'
#'
final_slash <- function(x){
  paste0(gsub("\\\\$","",x),"\\")
}



