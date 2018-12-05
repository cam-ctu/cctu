#' initialise  objects for using cctu package
#'
#' @param root the root directory to start in
#' @param scripts logical create the standard set of scripts
#' @return cctu_initialise gives an invisible return of logical indicating if the directories have been created. The directories needed are "Output", and within "Output", "Core", "Figures", "Reports".
#'
#' @seealso \code{\link{dir.create}}
#'
#' @export
#' @importFrom magrittr %>% %<>%

#' @describeIn cctu_initialise create the standard directories for outputs if needed.
cctu_initialise <- function(root=getwd(), scripts=FALSE){
  root %<>% normalizePath %>% final_slash
  reset_code_tree(root_file=paste0(root,"ROOT"))
  if( !cctu_check_dir(root=root)){
    dir.create(paste0(root, "Output")) &
      dir.create( paste0(root,"Output\\Core")) &
      dir.create( paste0(root, "Output\\Figures")) &
      dir.create( paste0(root, "Output\\Reports"))
  }
  if(scripts) {
    file.copy( system.file("doc/main.R",package="cctu"), root)
    file.copy( system.file("doc/Progs",package="cctu"), root, recursive=FALSE)
    print("Maybe set up a Project in Rstudio and a git repository?")
  } else { invisible(TRUE) }

  # Copy across the default set of scripts?
}

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


