#' Function to save ggplot figures.
#'
#' @param number the number used to as a suffix in the output filename, and to link to TableofTables.
#' @param plot the plot object to save. defaults to \code{last_plot}
#' @param width the width to save as
#' @param height the height to save as
#' @param dpi the resolution setting
#' @param units either "cm" (the default) or "inches"
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at the end. Defaults to TRUE
#' @param ... modifications to the default values of meta_table_string, reserved_string, popn_table_string see \code{\link{clean_up}}
#' @param directory where to save the figures within path or current working directory
#' @param path character string of the project filepath to save the table in. Default is NULL which implies the current working directory.
#' @inheritParams clean_up
#' @inheritParams add_program
#' @param format either "jpg", "postscript", or "png" to determine the file type to use
#' @param graphics_args a list of named arguments to supply to graphics function (png, postscript, jpeg)
#' @param frame the frame or environment in which lives the meta_table to be edited with teh path to the containing code file
#'
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables object with the calling programe No return object.
#' @seealso \code{\link{get_file_name}} \code{\link{write_table}}
#' @export
#' @importFrom magrittr %>%



write_ggplot = function(number,
                       plot     = last_plot(),
                       width    = 29.7 * 0.6,
                       height   = 21 * 0.6,
                       dpi      = 300,
                       units    = "cm",
                       clean_up = TRUE,
                       directory="Output/Figures/",
                       path=NULL,
                       ...,
                       format=c("png","postscript","jpeg"),
                       graphics_args=NULL,
                       frame=parent.frame()
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



  CallingProg <- get_file_name()
  if(is.null(CallingProg)){
    warning(paste("Unable to identify the code file that created figure", number))
    CallingProg <- "Missing"
  }
  add_program(number, CallingProg, frame=frame, ... )

  # deals with non-ggplot objects as well now

  format <- match.arg(format)
  file_name <- paste0(path, directory,"fig_",number)


  args_list <- c( list( file = paste0(file_name, ".", format %>% ifelse(.=="postcript","ps",.)),
                        height = height, width = width), graphics_args)
  extra_args <-NULL
  if( format %in% c("png","jpeg")){
    extra_args <- list(units = "in", res = dpi)
  }
  plotting_function <- getExportedValue("grDevices", format)
  do.call(plotting_function, c(args_list, extra_args))
  on.exit(utils::capture.output(grDevices::dev.off()))
  plot(plot)
  invisible()

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if(clean_up){
    clean_up(number, envir = frame, ...)
  }
}
