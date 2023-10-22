#' Function to save ggplot figures.
#' 
#' This is a wrapup function of \code{\link{write_plot}} to print \code{ggplot} object.
#'
#' @param number the number used to as a suffix in the output filename, and to link to TableofTables. Default is to use the value in the cctu_env package environment that is set within \code{\link{attach_pop}}.
#' @param plot the plot object to save. defaults to \code{last_plot}
#' @param width the width to save as
#' @param height the height to save as
#' @param dpi the resolution setting
#' @param units either "cm" (the default) or "inches"
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at the end. Defaults to TRUE
#' @param directory where to save the figures within path or current working directory
#' @inheritParams clean_up
#'
#' @param format either "jpg", "postscript", or "png" to determine the file type to use
#' @param graphics_args a list of named arguments to supply to graphics function (png, postscript, jpeg)
#' @param footnote character vector, can be used to add footnotes.
#'
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables object with the calling programe No return object.
#' @seealso  \code{\link{write_table}}  \code{\link{write_plot}}
#' @export
#' @importFrom magrittr %>% %<>%

write_ggplot = function(
                       plot     = last_plot(),
                       number = cctu_env$number,
                       width    = 29.7 * 0.6,
                       height   = 21 * 0.6,
                       dpi      = 300,
                       units    = "cm",
                       clean_up = TRUE,
                       directory=file.path("Output","Figures"),
                       format = c("png","postscript","jpeg"),
                       graphics_args=NULL,
                       verbose=options()$verbose,
                       footnote = NULL
                       ){

  # Collect all arguments
  args_list <- as.list(environment())
  args_list$plot_args <- list(x = ggplot_build(plot)$plot)
  args_list$plot <- NULL

  do.call(write_plot, args_list)
}

#' Function to save plot figures
#' 
#' One may not always use \code{ggplot2} to draw plot, base \code{plot} function for example,
#' this function is particularly useful in that situation. 
#'
#' @inheritParams write_ggplot
#'
#' @param plot_fn function to draw a plot.
#' @param plot_args A named list arguments for \code{plot_fn}.
#' 
#' @details
#' The \code{plot_fn} can be a user defined function to draw plot. All parameters should 
#' be passed with names. Checkout the examples below.
#' @examples
#' \dontrun{
#' # Below is a simple example.
#' write_plot(plot_fn = plot, plot_args = list(x = iris[,1], y = iris[,2]))
#' # This is equivalent drawing the following plot and save it
#' plot(x = iris[,1], y = iris[,2])
#' 
#' # Below is user defined function plotting
#' # One can use this method to draw a complicated plot 
#' new_plot <- function(x, y, h, v) {
#'    par(pty = "s", cex = 0.7) # adjust plot style
#'    plot(x, y)
#'    abline(h = h,v = v, lty=2) # add some lines
#' }
#' write_plot(plot_fn = new_plot, plot_args = list(x = iris[,1], y = iris[,2], h = 2.5, v = 6.0))
#' }
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables object with the calling programe No return object.
#' @seealso  \code{\link{write_table}} \code{\link{write_ggplot}}
#' @export
write_plot = function(
                      plot_fn  = plot,
                      number   = cctu_env$number,
                      width    = 29.7 * 0.6,
                      height   = 21 * 0.6,
                      dpi      = 300,
                      units    = "cm",
                      clean_up = TRUE,
                      directory=file.path("Output","Figures"),
                      format=c("png","postscript","jpeg"),
                      graphics_args=NULL,
                      verbose=options()$verbose,
                      footnote = NULL,
                      plot_args = NULL
                      ){


  # postscript() only takes units in inches
  if(units == "cm"){
    width  = width * 0.393701
    height = height * 0.393701
  }else if(units == "inches"){
    # do nothing
  }else{
    stop("units must be ''cm'' or ''inches''")
  }

  CallingProg <- cctu_env$parent[1]#get_file_name()
  if(is.null(CallingProg)){
    CallingProg <- "Missing"
    warning(paste("Unable to identify the code file that created figure", number))
  }
  add_program(number, CallingProg )
  
  if(!is.null(footnote))
    add_footnote(number, footnote)

  # deals with non-ggplot objects as well now

  format <- match.arg(format)
  #directory %<>% normalizePath %>% final_slash
  file_name <- file.path(directory,paste0("fig_",number))

  args_list <- c( list( file = paste0(file_name, ".", format %>% ifelse(.=="postscript","eps",.)),
                        height = height, width = width), graphics_args)
  extra_args <-NULL
  if( format %in% c("png","jpeg")){
    extra_args <- list(units = "in", res = dpi)
  }
  plotting_function <- getExportedValue("grDevices", format)
  do.call(plotting_function, c(args_list, extra_args))
  on.exit({
    utils::capture.output(grDevices::dev.off())
    if(verbose){cat("\n", args_list$file, "created.\n")}
    })
  do.call(plot_fn, plot_args)
  #

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if(clean_up){
    clean_up(number, frame = parent.frame(), verbose=verbose)
  }
  invisible()
}
