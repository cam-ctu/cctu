#' Produces the final Docx file
#'
#' @inheritParams create_word_xml
#' @param  figure_format it only supports \code{png} format.
#' @param debug print the folder name of the source files.
#' @export
#' @import xml2
#' @importFrom zip zipr
#' @importFrom xslt xml_xslt
#' @return This function is run for its side-effects: creates an xml document that glues together all the outputs and meta data as per the meta-table argument; a transformation fo this as per the xslt file, the default can be opened as a word document.
#'
#' @details  suggest that \code{\link{file.path}} is used to create non default file paths, to cope with OS vaguaries.

write_docx <- function(
    report_title,
    author,
    meta_table = get_meta_table(),
    filename = file.path("Output","Reports","Report.docx"),
    table_path = file.path("Output","Core"),
    figure_format = "png",
    figure_path = file.path("Output","Figures"),
    popn_labels = NULL,
    verbose = options()$verbose,
    debug = FALSE
){

  table_path <- normalizePath(table_path)
  long_filename <-  normalizePath(paste0(tools::file_path_sans_ext(filename), ".docx"), mustWork = FALSE)

  figure_format <- match.arg(figure_format)

  report_title <- remove_xml_specials(report_title)
  author <- remove_xml_specials(author)

  filename <- tempfile(fileext = ".xml")

  meta_table <- clean_meta_table(meta_table)

  if(!is.null(popn_labels)){
    #preserve any non-population based tables.
    index <- match(meta_table$population, unique(c("",meta_table$population)))
    meta_table$population <- c("", popn_labels)[index]
  }

  filename_text <- filename
  #create a connection to use in cat and
  filename <- file(description = filename, open = "a")

  output_dir <- tempdir(check = TRUE)

  if(debug)
    cat("Source files are stored at:\n", output_dir)

  # Avoid carrying over old files
  unlink(file.path(output_dir, "doc"), recursive = TRUE)

  # Copy folders and files
  invisible(
    file.copy(system.file("assets/doc", package="cctu"),
              output_dir,
              recursive = TRUE)
  )

  # Write Core
  core <- system.file("assets", "core.xml", package="cctu")
  core <- as_xml_document(core)
  ## Author
  my_node <- xml_find_first(core, xpath = "//dc:creator")
  xml_text(my_node) <- author
  ## Date created
  my_node <- xml_find_first(core, xpath = "//dcterms:created")
  xml_text(my_node) <- format(Sys.time(), format="%Y-%m-%dT%H:%M:%S+01:00")
  ## Document title
  my_node <- xml_find_first(core, xpath = "//dc:title")
  xml_text(my_node) <- report_title

  write_xml(core, file.path(output_dir, "doc/docProps/core.xml"))

  # Document content type
  doc_type <- system.file("assets", "[Content_Types].xml", package="cctu")
  doc_type <- as_xml_document(doc_type)

  # Document relationships
  doc_rels <- system.file("assets", "document.xml.rels", package="cctu")
  doc_rels <- as_xml_document(doc_rels)

  datestamp <- format(Sys.time(),format="%H:%M %d %b %Y")

  ## Header ID
  meta_table$headerid <- 1:nrow(meta_table) + 6

  ## Footer ID
  meta_table$footerid <- 1:nrow(meta_table) + max(meta_table$headerid)

  ## Figure ID
  fig_id <- meta_table$item == "figure"
  meta_table$figuerid <- cumsum(fig_id) + max(meta_table$footerid)
  meta_table$figuerid[meta_table$item != "figure"] <- NA

  cat(
    "\n <Report>\n<frontpage>
  <study>", report_title,"</study>
  <author>", author,"</author><datestamp>",
    datestamp, "</datestamp>\n</frontpage>",
    file = filename, append = TRUE)

  headers <- with(meta_table,
                 paste0("<heading><section>",  remove_xml_specials(as.character(section)),
                        "</section><title>",  remove_xml_specials(as.character(title)),
                        "</title><population>",
                        ifelse(is.na(population),"", remove_xml_specials(as.character(population))),
                        "</population><subtitle>",
                        ifelse(is.na(subtitle), "", remove_xml_specials(as.character(subtitle))),
                        "</subtitle><number>", number,
                        "</number><fontsize>",
                        ifelse(is.na(fontsize), "", remove_xml_specials(as.character(fontsize))),
                        "</fontsize></heading>\n",
                        sprintf("<pagesection><orientation>%s</orientation><headerid>rId%i</headerid><footerid>rId%i</footerid></pagesection>\n",
                                orientation, headerid, footerid)))

  # Format footnote
  footnote <- apply(meta_table, 1, function(x){
    r <- c(
      ifelse(is.na(x["footnote1"]), "", remove_xml_specials(as.character(x["footnote1"]))),
      ifelse(is.na(x["footnote2"]), "", remove_xml_specials(as.character(x["footnote2"])))
    )
    r <- r[r!=""]
    paste(r, collapse = "\n")
  })
  footnote <- paste("<footnote>", footnote, "</footnote>")

  # Write documents
  for(i in 1:length(headers)){

    cat("\n", file = filename, append = TRUE)

    if(meta_table[i, "item"] == "table"){
      cat("\n <MetaTable> \n", headers[i], file = filename, append = TRUE )
      table_text <- readLines(con = file.path(table_path,paste0('table_', meta_table[i, "number"], '.xml')))
      writeLines(table_text, con=filename)
      cat(footnote[i], "\n </MetaTable> \n", file = filename, append = TRUE)
    }

    if(meta_table[i, "item"] == "figure"){
      fig_path <- file.path(figure_path,
                            paste0("fig_", meta_table[i, "number"], ".", figure_format))
      fig_path <- normalizePath(fig_path)

      new_figname <- gsub("[^[:alnum:] ]", "",
                          tools::file_path_sans_ext(basename(fig_path)))
      new_figname <- paste0(new_figname, ".", tools::file_ext(fig_path))

      # Copy figure to median folder
      invisible(
        file.copy(fig_path,
                  file.path(output_dir,
                            sprintf("doc/word/media/%s", new_figname)))
      )

      # Add figure relationships
      xml_add_child(doc_rels, "Relationship",
                    Id = sprintf("rId%i", meta_table[i, "figuerid"]),
                    Type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
                    Target = sprintf("media/%s", new_figname))

      # Add figure to documents
      xml_add_child(doc_type, "Override",
                    PartName = sprintf("/word/media/%s", new_figname),
                    ContentType = "image/png")

      # Get image dimension and scale the figure to fit the page
      r <- png::readPNG(fig_path, native = TRUE, info = TRUE)
      img_wh <- attr(r, "info")$dim
      page_size <- c(595, 842)

      if(meta_table[i, "orientation"] == "landscape")
        page_size <- rev(page_size)

      # If the image is larger than page size
      if(page_size[1] < img_wh[1] | page_size[2] < img_wh[2])
        img_wh <- img_wh/max(img_wh/page_size + 0.5)

      # Pixel to EMU
      img_wh <- 9525*round(img_wh)

      cat(
        paste0("\n <MetaFigure> \n", headers[i],
               sprintf("<rid>%i</rid><width>%.0f</width><height>%.0f</height>",
                       meta_table[i, "figuerid"], img_wh[1], img_wh[2]),
               footnote[i], "\n </MetaFigure> \n"
        ),
        file = filename, append = TRUE
      )

    }

    if(meta_table[i, "item"] == "text"){
      cat("\n <MetaText> \n", headers[i], file = filename, append = TRUE)
      table_text <- readLines(con=file.path(table_path,paste0( 'text_', meta_table[i, "number"], '.xml')))
      writeLines(table_text, con=filename)
      cat(footnote[i], "\n </MetaText> \n", file = filename, append = TRUE)
    }

    # Write header
    header_xml <- to_wml.header(report_title, meta_table[i, "section"])
    write_xml(header_xml,
              file = file.path(output_dir, sprintf("doc/word/header%i.xml", i)))

    # Update relationships
    xml_add_child(doc_rels, "Relationship",
                  Id = sprintf("rId%i", meta_table[i, "headerid"]),
                  Type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/header",
                  Target = sprintf("header%i.xml", i))

    # Update documents
    xml_add_child(doc_type, "Override",
                  PartName = sprintf("/word/header%i.xml", i),
                  ContentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml")

    # write footer
    footer_xml <- to_wml.footer(author, meta_table[i, "program"])
    write_xml(footer_xml,
              file = file.path(output_dir, sprintf("doc/word/footer%i.xml", i)))

    # Update relationships
    xml_add_child(doc_rels, "Relationship",
                  Id = sprintf("rId%i", meta_table[i, "footerid"]),
                  Type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer",
                  Target = sprintf("footer%i.xml", i))

    # Update documents
    xml_add_child(doc_type, "Override",
                  PartName = sprintf("/word/footer%i.xml", i),
                  ContentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml")

  }

  writeLines("\n </Report>", con = filename )
  close(con = filename)

  #now apply the transform explicitly.
  xslt_file <- system.file("assets", "document.xslt", package="cctu")
  doc_xml <- xml2::read_xml(filename_text)
  transform <- xml2::read_xml(xslt_file)
  output <- xslt::xml_xslt(doc_xml, transform)
  xml2::write_xml(output, file = file.path(output_dir, "doc/word/document.xml"))

  # Write document content
  write_xml(doc_type, file.path(output_dir, "doc/[Content_Types].xml"))

  # Write document relationships
  write_xml(doc_rels, file.path(output_dir, "doc/word/_rels/document.xml.rels"))

  # zip files and generate DOCX
  curr_wd <- getwd()
  setwd(file.path(output_dir, "doc"))
  tryCatch(
    zip::zipr(zipfile = long_filename,
              include_directories = FALSE,
              files = list.files(path = ".", all.files = FALSE),
              recurse = TRUE)
    , error = function(e) {
      stop("Could not write ", shQuote(target), " [", e$message, "]")
    },
    finally = {
      setwd(curr_wd)
    })

  if(verbose){
    message(long_filename, " created.")
  }
}


# Generate header
#' @keywords internal
to_wml.header <- function(report_title, section){
  con <- system.file("assets", "header.xml", package="cctu")

  x <- read_xml(con)
  str <- sprintf("Tables Listing and Figures for %s | Section: %s",
                 "Demo report", "Baseline")
  nd <- xml_find_first(x, xpath = "//w:r")
  xml_text(nd) <- str
  x
}

# Generate footer
#' @keywords internal
to_wml.footer <- function(author, program){
  con <- system.file("assets", "footer.xml", package="cctu")

  x <- read_xml(con)
  str1 <- sprintf("Cambridge CTU, %s - %s - Page ",
                  author,
                  format(Sys.time(),format="%H:%M %d %b %Y"))
  str2 <- sprintf("Program: %s", program)

  nd1 <- xml_find_all(x, xpath = "//w:r/w:t[1]")
  xml_text(nd1) <- str1

  nd2 <- xml_find_all(x, xpath = "//w:r/w:t[2]")
  xml_text(nd2) <- str2
  x
}





