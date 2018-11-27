#' initialise the directories for using cctu package
#'
#' @param root the root directory to start in
#'
#' @return invisible return of logical indicating if the directories have been created. The directories needed are "Output", and within "Output", "Core", "Figures", "Reports".
#'
#' @seealso \code{\link{dir.create}}
#'
#' @export
#' @importFrom magrittr %>% %<>%


cctu_initialise <- function(root=getwd()){
  root %<>% normalizePath %>% final_slash
  reset_code_tree(root=paste0(root,"ROOT"))
  if( !cctu_check_dir(root=root)){
  dir.create(paste0(root, "Output")) &
    dir.create( paste0(root,"Output\\Core")) &
    dir.create( paste0(root, "Output\\Figures")) &
    dir.create( paste0(root, "Output\\Reports"))
  } else{ invisible(TRUE) }
}

#' @describeIn cctu_initialise Check if the directories exist for cctu
#' @param warnings logical indicator to issue warning if the directories do not exist. Default FALSE.
#' @return a logical indicating if the directories exist,
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


#' @describeIn cctu_initiliase reset the internal code_tree object to have no entries
#' @param root the name of the code file you want to use as the initial root for code tree
#' @export
reset_code_tree <- function(root="main.R"){
  cctu_env$code_tree <- data.frame(parent = as.character(NULL),
                                   child = as.character(NULL),
                                   stringsAsFactors = FALSE)
  cctu_env$parent <- root
}


