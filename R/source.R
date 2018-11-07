#' A modified version of the source() function to capture in a data.frame what file was called and from which file
#'
#'
#' @param code_tree_string character string giving the name of a data.frame to augment, or create if this is the first use
#' @param frame frame or environment in which to find data.frame named code_tree_string
#' @inheritParams base::source
#' @details evaluates code in the file argument, and also augments or creates a dataframe with two variables (parent, child).
#' The intention is to record the architecture of a sequenece of code files run using nested source() statements. To override this function use
#' \code{source <- base::source}.
#'
#' @return no return value
#' @seealso  \code{\link[base]{source}}
#' @export

source <- function(file, code_tree_string="code_tree", frame=parent.frame(), local=FALSE){
  parent_frame <- parent.frame()
  if(exists(code_tree_string, envir=frame)){
    code_tree <- get_obj(code_tree_string,frame=frame)
    envir <- where(code_tree_string, env=frame)
  }else{
    code_tree <- data.frame(parent=as.character(NULL),child=as.character(NULL), stringsAsFactors=FALSE)
    envir <- frame
  }
  parent <- get_parent()
  child <- normalizePath(file)
  code_tree_names <- names(code_tree)
  code_tree <- rbind(code_tree,
                     c("parent"=parent, "child"=child),
                     stringsAsFactors=FALSE
                     )
  names(code_tree) <- code_tree_names
  assign(code_tree_string, code_tree, envir= envir)
  base_source <- base::source
  #this function name will still match "source" in a regular expression used in get_file_name
 #eval( call( "base_source", file, echo=TRUE, max.deparse.length=Inf,local=local), envir=parent_frame)
  # Black magic why this works in testthat, but eval does not??
do.call(base_source, list( file=file, echo=TRUE, max.deparse.length=Inf, local=local), envir=parent_frame)

}



