#' Produces the final HTML, PDF or DOCX file
#'
#' This is for generating other document format type with Quarto. The created
#' \code{docx} and \code{pdf} documents may not have a nice output. Hence, the
#' \code{\link{write_docx}} should be used for the \code{docx} or \code{pdf}
#' by default. The PDF can be converted with Word. The rendering time for the
#' the HTML is much faster than PDF. Tables will be formated
#'  with \code{\link[flextable]{flextable}} and rendered with predifined Quarto
#'  templates using \code{\link[quarto]{quarto_render}}.
#'
#' @inheritParams create_word_xml
#' @param output_format Output format of the document, can be \code{"html"},
#' \code{"pdf"} or \code{"docx"}.
#' @param quiet Suppress warning and other messages from quarto,
#' see \code{\link[quarto]{quarto_render}}
#' @export
#' @import xml2
#' @importFrom xslt xml_xslt
#' @return This function is run for its side-effects: creates an HTML document.
#'
#' @details  suggest that \code{\link{file.path}} is used to create non default
#' file paths, to cope with OS vaguaries.


write_quarto <- function(
    report_title,
    author,
    meta_table = get_meta_table(),
    filename = file.path(
      getOption("cctu_output", default = "Output"),
      "Reports", "Report.html"
    ),
    table_path = file.path(
      getOption("cctu_output", default = "Output"),
      "Core"
    ),
    figure_path = file.path(
      getOption("cctu_output", default = "Output"),
      "Figures"
    ),
    output_format = c("html", "pdf", "docx"),
    popn_labels = NULL,
    verbose = options()$verbose,
    quiet = TRUE) {

  output_format <- match.arg(output_format)

  filename <- paste0(
    tools::file_path_sans_ext(filename),
    ".",
    output_format
  )

  meta_table <- clean_meta_table(meta_table)

  if (!is.null(popn_labels)) {
    # preserve any non-population based tables.
    index <- match(meta_table$population, unique(c("", meta_table$population)))
    meta_table$population <- c("", popn_labels)[index]
  }

  temp_file <- tempfile(fileext = ".rds")
  saveRDS(meta_table, file = temp_file)

  qmd_files <- list.files(system.file("assets/html", package = "cctu"),
                          pattern = "*.qmd")
  invisible(
    file.copy(file.path(system.file("assets/html", package = "cctu"), qmd_files),
              qmd_files,
              recursive = FALSE,
              overwrite = TRUE
    )
  )

  # HTML file output
  quarto::quarto_render(
    input =  "report.qmd",
    output_file = basename(filename),
    # execute_dir = file.path(getwd(), dirname(filename)),
    output_format = output_format,
    execute_params = list(meta_tbfl = temp_file,
                          title = report_title,
                          author = author,
                          table_path = table_path,
                          figure_path = figure_path),
    quiet = quiet
  )

  unlink(file.path(qmd_files))

  file.copy(from = basename(filename),
            to   = filename,
            overwrite = TRUE)

  unlink(file.path(basename(filename)))

  if (verbose) {
    message(filename, " created.")
  }
}

