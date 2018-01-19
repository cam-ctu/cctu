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
#' @param directory where to save the figures
#' @param path_string character string of the name of a global variable that contains the project filepath. Default is "PATH".
#' @inheritParams clean_up
#' @param format either "jpg", "postscript", or "png" to determine the file type to use
#' @param graphics_args a list of named arguments to supply to graphics function (png, postscript, jpeg)
#'
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables object with the calling programe No return object.
#' @seealso \code{\link{get_file_name}} \code{\link{write_table}}
#' @export



write_ggplot = function(number,
                       plot     = last_plot(),
                       width    = 29.7 * 0.6,
                       height   = 21 * 0.6,
                       dpi      = 300,
                       units    = "cm",
                       clean_up = TRUE,
                       directory="/Output/Figures",
                       path_string="PATH",
                       ...,
                       format=c("png","postscript","jpeg"),
                       graphics_args=NULL
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

  # TableofTables, PATH,  need to be defined in the environment
  # that calls this function

  PATH <- get_obj(path_string, alt=getwd())


  if(is.null(sys.calls())){
    CallingProg <- "Missing"
  } else {
    CallingProg <- get_file_name()
  }

  add_program(number, CallingProg, ... )

  # deals with non-ggplot objects as well now

  format <- match.arg(format)
  file_name <- paste0(PATH, directory,"/fig_",number)


  args_list <- c( list( file = paste0(file_name, ".", format),
                        height = height, width = width), graphics_args)
  extra_args <-ifelse( format %in% c("png","jpeg"),
                       list(units = "in", res = dpi),
                       NULL
                       )
  do.call(paste0("grDevices::",format), c(args_list, extra_args))
  on.exit(utils::capture.output(grDevices::dev.off()))
  plot(plot)
  invisible()

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if(clean_up){
    # this has to be called outside the function
    parent_frame <- parent.frame()
    clean_up(number, envir = parent_frame, ...)
  }
}
