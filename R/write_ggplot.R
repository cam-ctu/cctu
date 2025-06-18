
#' Function to save plot figures
#'
#' One may not always use \code{ggplot2} to draw plot, base \code{plot} function for example,
#' this function is particularly useful in that situation.
#'
#'
#' @param ... Arguments to be passed to the ploting function \code{plot_fn}
#' below.
#'
#' @param plot_fn function to draw a plot. This can be simple \code{print}
#' OR \code{plot} (default) OR a custom plot drawing function depending on how
#' the plot was drawn. See the \code{details} and \code{examples} below.
#' @param plot_args Deprecated, kept here for backcompatibility. A named list
#' arguments for \code{plot_fn}.
#' @param number the number used to as a suffix in the output filename, and to
#'  link to TableofTables. Default is to use the value in the cctu_env package
#'   environment that is set within \code{\link{attach_pop}}.
#' @param plot the plot object to save. defaults to \code{last_plot}
#' @param width the width to save as
#' @param height the height to save as
#' @param dpi the resolution setting
#' @param units either "cm" (the default) or "inches"
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at the
#'  end. Defaults to TRUE
#' @param directory where to save the figures within path or current working
#' directory. The Output directory can be over-riden with
#' options("cctu_output").
#' @inheritParams clean_up
#'
#' @param format either \code{"png"}, \code{"eps"}, \code{"ps"},
#' \code{"pdf"}, \code{"svg"}, \code{"jpg"} or \code{"jpeg"} to determine the
#'  file type to use. You can request multiple file types by providing a vector.
#'  The \code{"png"} format will always be produced and saved under
#'  \code{directory}, typically this is \code{Output/Figures}. All other formats
#'  will be saved under a sub-folder with the format name. See details for more
#'  information. You can set the default figure format by setting
#'  \code{options(cctu_fig_format = c("png", "eps"))} (default).
#' @param graphics_args a list of named arguments to supply to graphics
#' function. \link[grDevices]{png} for \code{"png"}, \link[grDevices]{postscript}
#' for \code{"ps"} or \code{"eps"}, \link[grDevices]{jpeg} for \code{"jpg"} and
#' \code{"jpeg"}, \link[grDevices]{pdf} for \code{"pdf"}). If you have the \code{ragg}
#' package installed, it will use \link[ragg]{agg_png} for \code{"png"} and
#' \link[ragg]{agg_jpeg} for \code{"jpg"} and \code{"jpeg"} for better quality.
#' The \link[svglite]{svglite} is used for \code{"svg"} file.
#' @param footnote character vector, can be used to add footnotes.
#'
#' @details
#'
#' ## Saving a plot
#'
#' The \code{plot_fn} can be a user defined function to draw plot. Let us
#' assume the object \code{p} is your plot object. If you can see the plot by
#' simply typing \code{p} on the console, then this should be \code{print}.
#' If you can see the plot by simply typing \code{plot(p)} on the console,
#' then this should be \code{plot}.
#'
#' All parameters should be passed with names. Checkout the examples below.
#'
#' ## Figure formats
#'
#' You can request for multiple figure formats when saving a plot. You can save
#' \code{"png"} (default), \code{"eps"}, \code{"ps"}, \code{"pdf"},
#' \code{"svg"}, \code{"jpg"} and/or \code{"jpeg"} formats. The \code{"png"}
#' format will always be produced to produce final Word report. The \code{"eps"},
#'  \code{"ps"}, \code{"pdf"} and \code{"svg"} are vector figures and you can
#'  post edit figures with tools like \href{https://inkscape.org/}{Inkscape}.
#'  The \code{"svg"} is recommended if you want to modify the plots. You can then
#' easily export \code{"svg"} to other formats, including \code{"eps"} and
#'  \code{"ps"}, \code{"pdf"}. The \link[svglite]{svglite} is used to generate
#' \code{"svg"} file.
#'
#' @examples
#' \dontrun{
#' #####################################
#' # Below is a simple example ========#
#' #####################################
#' #
#' write_plot(plot_fn = plot, plot_args = list(x = iris[, 1], y = iris[, 2]))
#' # This is equivalent drawing the following plot and save it
#' plot(x = iris[, 1], y = iris[, 2])
#'
#' # Below is user defined function plotting
#' # One can use this method to draw a complicated plot
#' new_plot <- function(x, y, h, v) {
#'   par(pty = "s", cex = 0.7) # adjust plot style
#'   plot(x, y)
#'   abline(h = h, v = v, lty = 2) # add some lines
#' }
#' write_plot(
#'   x = iris[, 1], y = iris[, 2], h = 2.5, v = 6.0,
#'   plot_fn = new_plot
#' )
#'
#'
#' ####################################################
#' # To draw a KM-plot from survminer package ========#
#' ####################################################
#'
#' library("survival")
#' library("survminer")
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' # Drawing survival curves
#' p <- ggsurvplot(fit, data = lung)
#' write_plot(p, plot_fn = survminer:::print.ggsurvplot)
#' # The code above works because the p is a ggsurvplot object (check it with class(p))
#' # There's a printing function print.ggsurvplot to handle the printing of the KM-plot.
#' # But this function is not exported by survminer, so we need to use three colons.
#'
#' #####################################
#' # Draw a consort diagram ===========#
#' #####################################
#'
#' library(grid)
#' # Might want to change some settings
#' txt0 <- c("Study 1 (n=160)", "Study 2 (n=140)")
#' txt1 <- "Population (n=300)"
#' txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=15)"
#'
#' # supports pipeline operator
#' g <- add_box(txt = txt0) |>
#'   add_box(txt = txt1) |>
#'   add_side_box(txt = txt1_side) |>
#'   add_box(txt = "Randomized (n=200)")
#' # Since you can draw the plot g with plot(g), the ploting function is plot
#' # The plotting function is \code{plot.consort}, so simple plot or plot.consort works
#' write_plot(g, plot_fn = plot)
#' # Or just
#' write_plot(g)
#' }
#' @return writes a copy of a plot to file fig_number. edits the TableofTables
#'  object with the calling program No return object.
#' @seealso  \code{\link{write_table}} \code{\link{write_ggplot}}
#' @export
write_plot <- function(...,
                       plot_fn = plot,
                       number = cctu_env$number,
                       width = 29.7 * 0.6,
                       height = 21 * 0.6,
                       dpi = 300,
                       units = "cm",
                       clean_up = TRUE,
                       directory = file.path(
                         getOption("cctu_output",
                           default = "Output"
                         ),
                         "Figures"
                       ),
                       format = getOption("cctu_fig_format", default = c("png", "eps")),
                       graphics_args = NULL,
                       verbose = options()$verbose,
                       footnote = NULL,
                       plot_args = NULL) {

  dot_args <- list(...)
  plot_args <- c(plot_args, dot_args)

  # postscript() only takes units in inches
  if (units == "cm") {
    width <- width * 0.393701
    height <- height * 0.393701
  } else if (units == "inches") {
    # do nothing
  } else {
    stop("units must be ''cm'' or ''inches''")
  }

  CallingProg <- cctu_env$parent[1] # get_file_name()
  if (is.null(CallingProg)) {
    CallingProg <- "Missing"
    warning(paste(
      "Unable to identify the code file that created figure",
      number
    ))
  }
  add_program(number, CallingProg)

  if (!is.null(footnote)) {
    add_footnote(number, footnote)
  }

  # Rename postscript to eps
  format <- ifelse(format == "postscript", "eps", format)

  # Output device for different formats
  eps <- function(filename, ...) {
    grDevices::postscript(file = filename, ...,
                          onefile = FALSE,
                          horizontal = FALSE,
                          paper = "special")
  }

  if (requireNamespace('ragg', quietly = TRUE)) {
    # Better figure output
    png_dev <- ragg::agg_png
    jpeg_dev <- ragg::agg_jpeg
    tiff_dev <- ragg::agg_tiff
  } else {
    png_dev <- grDevices::png
    jpeg_dev <- grDevices::jpeg
    tiff_dev <- grDevices::tiff
  }

  devices <- list(
    eps =  eps,
    ps =   eps,
    pdf =  function(filename, ...) grDevices::pdf(file = filename, ...),
    svg =  function(filename, ...)  svglite::svglite(file = filename, ...),
    png =  function(...) png_dev(..., res = dpi, units = "in"),
    jpg =  function(...) jpeg_dev(..., res = dpi, units = "in"),
    jpeg = function(...) jpeg_dev(..., res = dpi, units = "in")
  )

  if(any(!format %in% names(devices))){
    not_support <- format[!format %in% names(devices)]
    stop("Format(s) ", paste(not_support, collapse = ", "), " not supported!")
  }

  # Make sure PNG is included in the formats
  format <- unique(c("png", format))

  # Create sub-folder if does not exist
  if(any(!format %in% "png")){
    for(i in format[!format %in% "png"]){
      if(!dir.exists(file.path(directory, i)))
         dir.create(file.path(directory, i))
    }
  }

  # directory %<>% normalizePath %>% final_slash
  for(i in format){
    if(i == "png"){
      file_name <- file.path(directory, paste0("fig_", number))
    }else{
      file_name <- file.path(directory, i, paste0("fig_", number))
    }

    args_list <- c(list(
      filename = paste0(file_name, ".", i),
      height = height, width = width
    ), graphics_args)

    do.call(devices[[i]], args_list)
    do.call(plot_fn, plot_args)
    utils::capture.output(grDevices::dev.off())
    if (verbose) {
      cat("\n", args_list$filename, "created.\n")
    }
  }

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
  invisible()
}


#' Function to save ggplot figures.
#'
#' This is a wrapup function of \code{\link{write_plot}} to print \code{ggplot}
#'  object.
#'
#' @inheritParams write_plot
#' @param plot the plot object to save. defaults to \code{last_plot}
#'
#' @return writes a copy of a plot to file fig_number. edits the TableofTables
#'  object with the calling program No return object.
#' @seealso  \code{\link{write_table}}  \code{\link{write_plot}}
#' @export
#' @importFrom magrittr %>% %<>%

write_ggplot <- function(plot = last_plot(),
                         number = cctu_env$number,
                         width = 29.7 * 0.6,
                         height = 21 * 0.6,
                         dpi = 300,
                         units = "cm",
                         clean_up = TRUE,
                         directory = file.path(
                           getOption("cctu_output",
                                     default = "Output"
                           ),
                           "Figures"
                         ),
                         format = getOption("cctu_fig_format", default = "png"),
                         graphics_args = NULL,
                         verbose = options()$verbose,
                         footnote = NULL) {
  # Collect all arguments
  args_list <- as.list(environment())

  if (!inherits(plot, c("gtable", "gTree", "grob", "ggplot"))) {
    stop(
      "The plot is a not supported class of: ",
      paste(class(plot), collapse = ", "),
      ". Use `write_plot` function instead to draw this plot."
    )
  }

  names(args_list)[names(args_list) == "plot"] <- "x"
  args_list$plot_fn <- grid::grid.draw

  do.call(write_plot, args_list)
}


