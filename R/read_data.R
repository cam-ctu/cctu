#' Automatic reading in data from a meta-table of external data sets.
#'
#' @param x character string, the name of the object to be created, and referenced within the data to find the file path.
#' @param data_table data frame containing the meta-table of file paths of the external data files, and their desired R object names.
#' @param fun the function to be used to read in the data file. If unspecified it picks up file extensions ".xsl" and ".xslx" to use \code{readxl::read_xls} and \code{readxl::read_xlsx}, otherwise uses \code{read.csv}. This could actually be any function applied to the file path character string that is extracted from \code{data_table}, but a warning is issued if the function name does not contain "read".
#' @param frame Environment in which an object with name given by \code{x} is created. Default is parent.frame(). Or if NULL the data read in is returned with no assignement.
#' @param name_variable character string giving the variable name within \code{data} that has the object names to be referenced. Defaults to "name".
#' @param file_variable character string giving the variable name within \code{data} that has the file names to be referenced. Defaults to "file".
#' @param ... other arguments to supply to \code{fun}.
#'
#' @return \code{read_data} assigns or returns a data frame reading in data from an external file
#'
#' @details The idea is to improve the tracibility of reading in external data.
#' This should be used in two steps: create a meta-table in R that has a minimum of 2 columns, one with the name of the R data.frame
#' to be created, and the other giving the file path to the external data; use \code{read_data} as a wrapper to read in the data as specified. This ends up with less code, and allows an table of extenral data and associated meta-data to
#' be easily produced using \code{data_table_summary}. If options("verbose") is TRUE then \code{read_data} will display messages describing what objects have been created.
#'
#' @examples
#' data_table <- data.frame(name=c("dirtydata","meta"),
#'                          file=c(system.file("extdata","dirtydata.csv", package="cctu"),
#'                                 system.file("extdata","meta_table.xlsx", package="cctu")),
#'                          folder="")
#' data_table_summary(data_table)
#' options("verbose"=TRUE)
#' for( obj in data_table$name){ read_data(obj, data_table) }
#' summary(dirtydata)
#' summary(meta)
#'
#' @export
#' @seealso \code{\link{read.csv}} \code{\link[readxl]{read_xls}} \code{\link[readxl]{read_xlsx}}



#' @describeIn read_data reads in data from external files using a meta-table of file names and objects names to be created.
read_data <- function(x,
                      data_table,
                      fun=NULL,
                      frame=parent.frame(),
                      name_variable="name",
                      file_variable="file",
                      ...){
  names <- data_table[,name_variable]
  files <- data_table[,file_variable]
  file <- as.character(files[match(x, names)])

  if(is.null(fun)){
    fun <- utils::read.csv
    # some sort of intelligent setting of fun
    if( grepl("\\.xlsx?$", file) ){
      if(!requireNamespace("readxl",quietly = TRUE) ){stop("Package \"readxl\" needed for this function to load excel files")}
      fun <- readxl::read_excel
    }
  }else if( !grepl("read",deparse(substitute(fun)))){
    warning(
      paste("this function is designed to be used for reading in data. You are calling:",
            deparse(substitute(fun))
      )
    )}

   output <- do.call(fun, c(list(file), ...=...))
  if( is.null(frame)){
    output
  }else{
    assign(x, output,envir = frame)
    if(options()$verbose){ cat(paste0("object created in ",environmentName(frame),": ", x,"\n"))}
  }
}

#' @describeIn read_data produce a summary of the meta-table giving the external data files.
#' @inheritParams read_data
#' @param folder_variable character string giving the variable name within \code{data} that contains the folder to find the external file. \code{data_table_summary} Looks up \code{getwd()} if empty.
#' @param mod_time_variable character string given the variable name to be created in \code{data_table_summary} that records the time stamp of when the external file was last modified.
#'
#' @return \code{data_table_summary} returns a data frame sumarising the meta-table and the associated information about time of last modification and full file paths.
#'
#' @export



data_table_summary <- function(data_table,
                             name_variable="name",
                             file_variable="file",
                             folder_variable="folder",
                             mod_time_variable="mod_time"
){
  folder <- data_table[,folder_variable]
  folder <- ifelse(is.na(folder)|folder=="", getwd(), normalizePath(folder))
  full_path <- file.path(folder, data_table[,file_variable])
  mod_time= file.mtime(full_path)
  X <- data.frame(data_table[,name_variable],data_table[,file_variable], folder,  mod_time, full_path)
  names(X) <- c(name_variable, file_variable, folder_variable,mod_time_variable, "full_file_path")
  X
}

