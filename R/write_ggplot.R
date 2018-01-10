#' Function to save ggplot figures.
#'
#' @param number the figure number used to link to the meta data file and used to name the file fig_"number"
#' @param plot the plot object to save. defaults to \code{last_plot}
#' @param width the width to save as
#' @param height the height to save as
#' @param dpi the resolution setting
#' @param units either "cm" (the default) or "inches"
#' @param clean_up logical to invoke the \code{\link{clean_up}} function at the end. Defaults to TRUE
#' @param ... arguments to feed into the printing function
#' @param directory where to save the figures
#' @param format either "jpg", "postscript", or "png" to determine the file type to use
#'
#' @return writes a copy of a plot to file fig_number.. edits the TableofTables object with the calling programe No return object.
#' @seealso \code{\link{get_file_name}} \code{\link{write_table}}



write_ggplot = function(number,
                       plot     = last_plot(),
                       width    = 29.7 * 0.6,
                       height   = 21 * 0.6,
                       dpi      = 300,
                       units    = "cm",
                       clean_up = TRUE,
                       directory="/Output/Figures",
                       format=c("png","postscript","jpg"),
                       ...
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

  if(exists("PATH")){
    PATH <- eval(as.name("PATH"))
  }else{
    warning("''PATH'' was not defined in the parent environment")
    PATH <- getwd()
  }

  if(is.null(sys.calls())){
    CallingProg <- "Missing"
  } else {
    CallingProg <- get_file_name()
  }


  if( exists("TableofTables")){
    TableofTables <- eval(as.name("TableofTables"))
  TableofTables[!is.na(TableofTables$Number) & as.character(TableofTables$Number) == number, "Program"] <- CallingProg
  assign("TableofTables", TableofTables, envir = .GlobalEnv)
  } else{
    warning("''TableofTables'' was not defined in the parent frame")
  }
  # deals with non-ggplot objects as well now

  format <- match.arg(format)
  file_name <- paste0(PATH, directory,"/fig_",number)

  switch( format,
  png=grDevices::png(file = paste0(file_name, ".png"),
                 height = height, width = width, units = "in", res = dpi, ...),
  postscript=grDevices::postscript(file=paste0(file_name,".eps"),
                        height=height, width=width,  ...),
  jpg=grDevices::jpeg(file=paste0(file_name,".jpg"),
                       height=height, width=width, units="in", res=dpi, ...)
)
  on.exit(utils::capture.output(grDevices::dev.off()))
  plot(plot)
  invisible()

  # this links in with using environments to define the correct population
  # detach_pop(number)
  if(clean_up){
    # this has to be called outside the function
    parent_frame <- parent.frame()
    clean_up(number, envir = parent_frame)
  }
}
