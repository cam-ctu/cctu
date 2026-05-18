#' Produces the final xml file
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is no longer maintained. Use \code{\link{write_docx}} instead.
#'
#' @param report_title text string used to label the report title page
#' @param author text string naming the author
#' @param meta_table a data frame  that contains meta information on tables
#' (title, population, number). Defaults is get_meta_table()
#' @param datestamp text used to give the date-time stamp, defaults to the
#' system date/time at the time of running the function
#' @param filename text string giving the filename/path to output the word
#' document to
#' @inheritParams write_ggplot
#' @inheritParams attach_pop
#' @param table_path text string giving the filepath to the tables folder.
#' This is used to directly open table files and copy the content.
#' @param figure_path text string giving the filepath to the figures folder.
#' This is used to create a reference within the  word document. Hence it needs
#'  to be a relative (to the output report) path "../Figures". Likely to lead
#'   to empty figures in the report if it is changed.
#' @param popn_labels alternative text string giving labels used for the
#' population - might want to include the population size... They must match
#'  correctly to unique(meta_table$population), excluding rows with a blank,
#'  or no, population given
#' @param  figure_format the format to look for figure files when building the
#'  report ("png", "jpeg","ps")
#' @param xslt_file a text file containing the xslt document. Default is
#' system.file("extdata", "xml_to_word.xslt", package="cctu").
#' @param keep_xml a boolean if the compiled XML should be kept, used for
#'  debugging purpose.
#' @export
#'
#' @return This function is run for its side-effects: creates an xml document
#'  that glues together all the outputs and meta data as per the meta-table
#'   argument; a transformation fo this as per the xslt file, the default can
#'   be opened as a word document.
#'
#' @details  suggest that \code{\link{file.path}} is used to create non default
#'  file paths, to cope with OS vaguaries.


create_word_xml <- function(
  report_title,
  author,
  meta_table = get_meta_table(),
  datestamp = format(Sys.time(), format = "%H:%M %d %b %Y"),
  filename = file.path(cctu_opt("output"), "Reports", "Report.doc"),
  table_path = file.path(cctu_opt("output"), "Core"),
  figure_format = c("png", "jpeg", "ps"),
  figure_path = file.path(cctu_opt("output"), "Figures"),
  popn_labels = NULL,
  verbose = options()$verbose,
  xslt_file = system.file("extdata", "to_word.xslt", package = "cctu"),
  keep_xml = FALSE
) {
  lifecycle::deprecate_warn("0.8.12", "create_word_xml()", "write_docx()",
                            always = testthat::is_testing()
                            )

  write_docx(
    report_title = report_title,
    author = author,
    meta_table = meta_table,
    filename = filename,
    table_path = table_path,
    figure_format = figure_format,
    figure_path = figure_path,
    popn_labels = popn_labels,
    verbose = verbose,
    keep_xml = keep_xml
  )
}

