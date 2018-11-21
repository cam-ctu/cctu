#' Produces the final xml file
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param meta_table a data frame  that contains meta information on tables (title, population, number)
#' @param datestamp text used to give the date-time stampe, defaults to the system date/time at the time of running the function
#' @param filename text string giving the filename/path to output the word document to
#' @param path text string giving the parent folder in which the output file, table_path and figure_path are held. Defaults to the current working directory.
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables
#' @param figure_path text string giving the filepath to the figures
#' @param popn_labels alternative text string giving labels used for the population - might want to include the population size... They must match correctly to unique(meta_table$population), excluding rows with a blank, or no, population given
#' @param  figure_format the format to look for figure files when building the report ("png", "jpeg","ps")
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
  path=getwd(),
  table_path="Output\\Core\\",
  figure_format=c("png","jpeg","ps"),
  figure_path="Output\\Figures\\",
  frame=parent.frame(),
  popn_labels=NULL
){
  #PATH <- get_obj(path_string,frame=frame, alt="Missing Frame")
  #check you are in the right working directory

  if(getwd()!=path){warning(paste("you are calling create_word_xml with the working directory not equal to", path))}
  #manage paths to deal with trailing slashes or not...

  path %<>% normalizePath %>% final_slash
  figure_path %<>% normalizePath %>% final_slash
  table_path %<>% normalizePath %>% final_slash

  ## CHekcs don't like this, but are OK using with(.)
    #meta_table = meta_table[!is.na(meta_table$Number), ]

  meta_table <- clean_meta_table(meta_table)

  if(!is.null(popn_labels)){
    #preserve any non-population based tables.
    index <- match(meta_table$population, unique(c("",meta_table$population)))
    meta_table$population <- c("", popn_labels)[index]
  }



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
      call = paste0('type "',table_path,'table_', meta_table[i, "number"], '.xml" >> "',
                    filename, '"')
      shell(call)
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
      call = paste0('type "',table_path,'text_', meta_table[i, "number"], '.xml" >> "',
                    filename, '"')
      shell(call)
      cat(footers[i], program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  cat("\n </Report>", file = filename, append = TRUE)

}

#' @keywords internal
#'
#'
final_slash <- function(x){
  paste0(gsub("\\\\$","",x),"\\")
}



#' @keywords internal
#'

clean_meta_table <- function(meta_table){
  op <- options()
  options(stringsAsFactors = FALSE)
  columns_needed <- c("section","title","subtitle","number","population",
                      "orientation", "program", "item", "footnote1","footnote2")



  if( !("number" %in% names(meta_table))){warning("Need to have 'number' column in meta_table")}
  if( !("item" %in% names(meta_table))){warning("Need to have 'item' column meta_table")}

  meta_table$number <- gsub("\\s","", meta_table$number)
  pmat <- pmatch(names(meta_table), columns_needed)
  meta_table <- subset( meta_table, !is.na(meta_table$number) & meta_table$number!="", select=!is.na(pmat))
  pmat <- pmatch(names(meta_table), columns_needed)
  names(meta_table) <- columns_needed[pmat]
  index <- meta_table$number %>% as.character %>% order_dewey
  meta_table <- meta_table[index,]
  n <- nrow(meta_table)

  extra_cols <- columns_needed[-pmat]
  if(length(extra_cols)){
    X <- matrix(" ", nrow=n, ncol=length(extra_cols))
    X <- as.data.frame(X)
    names(X) <- extra_cols
    meta_table <- cbind(meta_table, X)
  }

  meta_table$population <- gsub("\\s","", meta_table$population)

  if( "orientation" %in% extra_cols){
    meta_table$orientation <- "portrait"
  }

  item_index <- pmatch(meta_table$item %>% tolower,
                       c("table","figure","text"),
                       duplicates.ok = TRUE)
  if(any(is.na(item_index))){
    warning("invalid values for 'item' converted to 'table'")
    item_index <- ifelse(is.na(item_index),1, item_index)
    }
  meta_table$item <- c("table","figure","text")[item_index]

  orient_index <- pmatch(meta_table$orientation %>% tolower,
                         c("portrait", "landscape"),
                         duplicates.ok=TRUE)
  if(any(is.na(orient_index))){
    warning("invalid values for 'orientation' converted to 'portrait'")
    orient_index <- ifelse(is.na(orient_index), 1, orient_index)
    }
  meta_table$orientation <- c("portrait", "landscape")[orient_index]
  options(op)
  meta_table
}


