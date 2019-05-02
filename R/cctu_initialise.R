#' initialise  objects for using cctu package
#'
#' @param root the root directory to start in
#' @param scripts logical create the standard set of scripts. Intended to be used once interactively at the start of coding for an analysis.
#' @param rm logical whether to also run \code{\link{rm_output}} with its default values, to delete all the files in the output directory.
#' @return cctu_initialise gives an invisible return of logical indicating if the directories have been created. The directories needed are "Output", and within "Output", "Core", "Figures", "Reports".
#'
#' @seealso \code{\link{dir.create}}
#'
#' @export
#' @importFrom magrittr %>% %<>%

#' @describeIn cctu_initialise create the standard directories for outputs if needed.
cctu_initialise <- function(root=getwd(), scripts=FALSE, rm=FALSE){
  root_no_slash <- root %>% normalizePath
  root <- root_no_slash %>% final_slash
  reset_code_tree(root_file=paste0(root,"ROOT"))
  if( !cctu_check_dir(root=root)){
    dir.create(paste0(root, "Output")) &
      dir.create( paste0(root,"Output\\Core")) &
      dir.create( paste0(root, "Output\\Figures")) &
      dir.create( paste0(root, "Output\\Reports"))
  }
  if(scripts) {
    file.copy( system.file("scripts\\main.R",package="cctu"), root_no_slash)
    file.copy( system.file("scripts\\Progs",package="cctu"), root_no_slash, recursive=TRUE)
    dir.create(paste0(root, "library"))
    print("Maybe set up a Project in Rstudio and a git repository?\nCopy across or install packages in the project library, set .libPaths()?")
  }
  if(rm){ rm_output()}

}


# spelling variant
#' @describeIn cctu_initialise identifical function with american spelling
#' @export
cctu_initialize <- cctu_initialise


#' @describeIn cctu_initialise Check if the directories exist for cctu
#' @param warnings logical indicator to issue warning if the directories do not exist. Default FALSE.
#' @return cctu_check_dir gives a logical indicating if the directories exist,
#' @export

cctu_check_dir <- function(root=getwd(), warnings=FALSE){
  root %<>% normalizePath %>% final_slash
  check <- dir.exists(paste0(root, "Output")) &
    dir.exists( paste0(root,"Output\\Core")) &
    dir.exists( paste0(root, "Output\\Figures")) &
    dir.exists( paste0(root, "Output\\Reports"))
  if(warnings && !check){ warning("Default directories needed by cctu do not exist")}
  check
}



#' @describeIn cctu_initialise reset the internal code_tree object to have no entries
#' @param root_file the name of the code file you want to use as the initial root for code tree
#' @export
reset_code_tree <- function(root_file="main.R"){
  cctu_env$code_tree <- data.frame(parent = as.character(NULL),
                                   child = as.character(NULL),
                                   stringsAsFactors = FALSE)
  cctu_env$parent <- root_file

}

#' @describeIn cctu_initialise function to clear previous output files, but leaves the directories in place
#'
#' @param root_output character giving the output directory path
#' @param core logical delete the files in /Core
#' @param figures logical delete the files /Figures
#' @param reports logical delete the files in /Reports
#' @param output logical delete top level files in
#' @export



rm_output <- function(root_output="Output", core=TRUE, figures=TRUE, reports=TRUE, output=TRUE){
  if(output){
    files <- list.files(root_output, recursive=FALSE)
    dirs <- list.dirs(root_output)[-1]
    dirs <- gsub(paste0(root_output,"/"),"", dirs)
    for(file in files){
      if( !(file %in% c("Core","Figures","Reports",dirs))){
        file.remove(paste0(root_output,"\\",file))
      }
    }
  }

  if( core){
    files <- list.files(paste0(root_output,"\\Core"))
    for( file in files){ file.remove(paste0(root_output,"\\Core\\",file))}
  }
  if( figures){
    files <- list.files(paste0(root_output,"\\Figures"))
    for( file in files){ file.remove(paste0(root_output,"\\Figures\\",file))}
  }
  if( reports){
    files <- list.files(paste0(root_output,"\\Reports"))
    for( file in files){ file.remove(paste0(root_output,"\\Reports\\",file))}
  }

}

