#' Provides a summary with date stamps of external data files.
#'
#' @inheritParams read_data
#' @param folder_variable character string giving the variable name within \code{data} that contains the folder to find the external file. \code{data_table_summary} Looks up \code{getwd()} if empty.
#' @param mod_time_variable character string given the variable name to be created in \code{data_table_summary} that records the time stamp of when the external file was last modified.
#'
#' @return \code{data_table_summary} returns a data frame sumarising the meta-table and the associated information about time of last modification and full file paths.
#' @seealso \code{\link{read_data}}
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
  folder <- gsub('\\\\',"/", folder)
  full_path <- file.path(folder, data_table[,file_variable]) %>% normalizePath(winslash = "/")
  full_path <- gsub('\\\\',"/", full_path)
  mod_time= file.mtime(full_path)
  X <- data.frame(data_table[,name_variable],data_table[,file_variable], folder,  mod_time, full_path)
  names(X) <- c(name_variable, file_variable, folder_variable,mod_time_variable, "full_file_path")
  X
}

