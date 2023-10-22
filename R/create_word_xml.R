#' Produces the final xml file
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param meta_table a data frame  that contains meta information on tables (title, population, number). Defaults is get_meta_table()
#' @param datestamp text used to give the date-time stamp, defaults to the system date/time at the time of running the function
#' @param filename text string giving the filename/path to output the word document to
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables folder. This is used to directly open table files and copy the content.
#' @param figure_path text string giving the filepath to the figures folder. This is used to create a reference within the  word document. Hence it needs to be a relative (to the output report) path "../Figures". Likely to lead to empty figures in the report if it is changed.
#' @param popn_labels alternative text string giving labels used for the population - might want to include the population size... They must match correctly to unique(meta_table$population), excluding rows with a blank, or no, population given
#' @param  figure_format the format to look for figure files when building the report ("png", "jpeg","ps")
#' @param xslt_file a text file containing the xslt document. Default is system.file("extdata", "xml_to_word.xslt", package="cctu").
#' @param keep_xml a boolean if the compiled XML should be kept, used for debugging purpose.
#' @export
#' @importFrom magrittr %>% %<>%
#'
#' @return This function is run for its side-effects: creates an xml document that glues together all the outputs and meta data as per the meta-table argument; a transformation fo this as per the xslt file, the default can be opened as a word document.
#'
#' @details  suggest that \code{\link{file.path}} is used to create non default file paths, to cope with OS vaguaries.




create_word_xml <- function(
  report_title,
  author,
  meta_table=get_meta_table(),
  datestamp=format(Sys.time(),format="%H:%M %d %b %Y"),
  filename=file.path("Output","Reports","Report.doc"),
  table_path=file.path("Output","Core"),
  figure_format=c("png","jpeg","ps"),
  figure_path=file.path("Output","Figures"),
  popn_labels=NULL,
  verbose=options()$verbose,
  xslt_file=system.file("extdata", "to_word.xslt", package="cctu"),
  keep_xml = FALSE
){

  table_path %<>% normalizePath #%>% final_slash
  long_filename <-  filename %>% normalizePath(., mustWork=FALSE)

  if(keep_xml)
    filename %<>% paste0(.,".xml")
  else
    filename <- tempfile(fileext = ".xml")

  meta_table <- clean_meta_table(meta_table)

  if(!is.null(popn_labels)){
    #preserve any non-population based tables.
    index <- match(meta_table$population, unique(c("",meta_table$population)))
    meta_table$population <- c("", popn_labels)[index]
  }

  file.copy( system.file("extdata", "header.txt", package="cctu") ,
             filename, overwrite=TRUE
  )

  filename_text <- filename
  #create a connection to use in cat and
  filename <- file(description=filename, open="a")


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
                        "</number><fontsize>",
                        ifelse(is.na(fontsize), "", remove_xml_specials(as.character(fontsize))),
                        "</fontsize><orientation>", orientation,
                        "</orientation></heading>"))

  # Format footers
  footers <- apply(meta_table, 1, function(x){
    r <- c(
      ifelse(is.na(x["footnote1"]), "", remove_xml_specials(as.character(x["footnote1"]))),
      ifelse(is.na(x["footnote2"]), "", remove_xml_specials(as.character(x["footnote2"])))
    )
    r <- r[r!=""]
    paste(r, collapse = "\n")
  })
  footers <- paste("<footnote>", footers, "</footnote>")

  program =  paste("<Program>", meta_table$program, "</Program>")
  figure_format <- match.arg(figure_format)

  for(i in 1:length(headers)){
    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "item"] == "table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      table_text <- readLines( con=file.path(table_path,paste0( 'table_', meta_table[i, "number"], '.xml')))
      writeLines(table_text, con=filename)
      #file.append(filename, file.path(table_path,paste0( 'table_', meta_table[i, "number"], '.xml')))
      cat(footers[i], program[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }
    if(meta_table[i, "item"] == "figure"){
      fig_path <- file.path(figure_path,
                            paste0("fig_", meta_table[i, "number"], ".", figure_format))
      fig_path <- normalizePath(fig_path)

      # Get image dimension and scale the figure to fit the page
      img_wh <- get_image_dim(fig_path)
      page_size <- c(595, 842)

      if(meta_table[i, "orientation"] == "landscape")
        page_size <- rev(page_size)

      # If the image is larger than page size
      if(page_size[1] < img_wh[1] | page_size[2] < img_wh[2])
        img_wh <- img_wh/max(img_wh/page_size + 0.5)

      cat("\n <MetaFigure> \n", headers[i], file = filename, append = TRUE)
      cat(sprintf("<src>%s</src>", basename(fig_path)),
          file = filename, append = TRUE)
      cat(sprintf("<figBase64>%s</figBase64>", base64enc::base64encode(fig_path)),
          file = filename, append = TRUE)
      cat(sprintf("<figuresize>width:%.2fpx;height:%.2fpx</figuresize>", img_wh[1], img_wh[2]),
          file = filename, append = TRUE)
      cat(footers[i], program[i], "\n </MetaFigure> \n", file = filename, append = TRUE)
    }

    if(meta_table[i, "item"] == "text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      table_text <- readLines( con=file.path(table_path,paste0( 'text_', meta_table[i, "number"], '.xml')))
      writeLines(table_text, con=filename)
      #file.append(filename, file.path(table_path,paste0('text_', meta_table[i, "number"], '.xml')))
      cat(footers[i], program[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }
  }


  #cat("\n </Report>", file = filename, append = TRUE)
  writeLines("\n </Report>", con = filename )
  close(con=filename)
  #now apply the transform explicitly.
  doc <- xml2::read_xml(filename_text)
  transform <- xml2::read_xml(xslt_file)
  output <- xslt::xml_xslt(doc, transform)
  xml2::write_xml(output, file=long_filename)

  if(verbose){
    message(long_filename, " created.")
  }
}

#' @keywords internal
#'
#'
final_slash <- function(x){
  paste0(gsub("\\\\$","",x),"\\")
}

#' @keywords internal
#'
get_image_dim <- function(path) {
  # Ensure file exists
  if(!file.exists(path))
    stop("No file found", call. = FALSE)

  # Ensure file ends with .png or .jpg or jpeg
  if (!grepl("\\.(png|jpg|jpeg)$", x = path, ignore.case = TRUE))
    stop("File must end with .png, .jpg, or .jpeg", call. = FALSE)

  # Get return of file system command
  s <- system(paste0("file ", path), intern = TRUE)

  # Extract width and height from string
  width <- regmatches(s, gregexpr("(?<=, )[0-9]+(?=(x| x )[0-9]+,)", s, perl = TRUE))[[1]]
  height <- regmatches(s, gregexpr(", [0-9]+(x| x )\\K[0-9]+(?=,)", s, perl = TRUE))[[1]]
  setNames(as.numeric(c(width, height)), c("Width", "Height"))
}

