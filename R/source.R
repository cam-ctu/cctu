#' A modified version of the source() function to capture in a data.frame what file was called and from which file
#'
#'
#' @inheritParams base::source
#' @param backup A character string giving the file path to which to save an image of the R environment before sourcing any code.
#' Default is NULL, which does not save any image.
#' @details evaluates code in the file argument, and also augments or creates a dataframe with two variables (parent, child).
#' The intention is to record the architecture of a sequenece of code files run using nested source() statements. To override this function use
#' \code{source <- base::source}.
#'
#' @return no return value
#' @seealso  \code{\link[base]{source}}
#' @export

source <- function(file, local=FALSE, backup=NULL
                   #code_tree_string="code_tree", frame=parent.frame()
                   ){
  parent_frame <- parent.frame()
  if(!is.null(backup)){save.image(file=backup)}
  code_tree <- cctu_env$code_tree
  child <- normalizePath(file)
  code_tree[1+nrow(code_tree),] <- c(cctu_env$parent[1], child)
  cctu_env$code_tree <- code_tree
  cctu_env$parent <- c(child, cctu_env$parent)
  base_source <- base::source
  #this function name will still match "source" in a regular expression used in get_file_name
  #evalq( call( "base_source", file=file, echo=TRUE, max.deparse.length=Inf,local=local), envir=parent_frame)
  # evalq works but eval does not...
  do.call(base_source, list( file=file, echo=TRUE, max.deparse.length=Inf, local=local), envir=parent_frame)
  cctu_env$parent <- cctu_env$parent[-1]
}



