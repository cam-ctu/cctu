#' Function to save ggplot figures.
#'
#' This is a wrapup function of \code{\link{write_plot}} to print \code{ggplot}
#'  object.
#'
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
#' @param format either "jpg", "postscript", or "png" to determine the file
#' type to use
#' @param graphics_args a list of named arguments to supply to graphics
#' function (png, postscript, jpeg)
#' @param footnote character vector, can be used to add footnotes.
#'
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables
#'  object with the calling programe No return object.
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
                         format = c("png", "postscript", "jpeg"),
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

#' Function to save plot figures
#'
#' One may not always use \code{ggplot2} to draw plot, base \code{plot} function for example,
#' this function is particularly useful in that situation.
#'
#' @inheritParams write_ggplot
#'
#' @param ... Arguments to be passed to the ploting function \code{plot_fn}
#' below.
#'
#' @param plot_fn function to draw a plot. This can be simple \code{print}
#' OR \code{plot} (default) OR a custom plot drawing function depending on how
#' the plot was drawn. See the \code{details} and \code{examples} below.
#' @param plot_args Deprecated, kept here for backcompatibility. A named list
#' arguments for \code{plot_fn}.
#'
#' @details
#' The \code{plot_fn} can be a user defined function to draw plot. Let us
#' assume the object \code{p} is your plot object. If you can see the plot by
#' simply typing \code{p} on the console, then this should be \code{print}.
#' If you can see the plot by simply typing \code{plot(p)} on the console,
#' then this should be \code{plot}.
#'
#' All parameters should be passed with names. Checkout the examples below.
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
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables
#'  object with the calling programe No return object.
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
                       format = c("png", "postscript", "jpeg"),
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

  # deals with non-ggplot objects as well now

  format <- match.arg(format)
  # directory %<>% normalizePath %>% final_slash
  file_name <- file.path(directory, paste0("fig_", number))

  args_list <- c(list(
    file = paste0(file_name, ".", format %>%
      ifelse(. == "postscript", "eps", .)),
    height = height, width = width
  ), graphics_args)
  extra_args <- NULL
  if (format %in% c("png", "jpeg")) {
    extra_args <- list(units = "in", res = dpi)
  }
  plotting_function <- getExportedValue("grDevices", format)
  do.call(plotting_function, c(args_list, extra_args))
  on.exit({
    utils::capture.output(grDevices::dev.off())
    if (verbose) {
      cat("\n", args_list$file, "created.\n")
    }
  })
  do.call(plot_fn, plot_args)
  #

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if (clean_up) {
    clean_up(number, frame = parent.frame(), verbose = verbose)
  }
  invisible()
}
