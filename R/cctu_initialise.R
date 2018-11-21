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
  dir.create(paste0(root, "Output")) &
    dir.create( paste0(root,"Output\\Core")) &
    dir.create( paste0(root, "Output\\Figures")) &
    dir.create( paste0(root, "Output\\Reports"))

}
