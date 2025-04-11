#' track which files of code are called by which oher files
#'
#' @return the data file containing two variables: parent ; child.
#'
#' @details each time the \code{cctu} function \code{\link{source}} is called,
#' an internal data frame is appended with the parent file that called
#' \code{\link{source}}, and the child file that was sourced.
#'
#' You can reset the code_tree or examine it, but not directly edit it. It is
#' reset automatically if
#' \code{\link{cctu_initialise}} is called.
#'
#' @seealso \code{\link{cctu_initialise}}
#' @rdname get_code_tree
#' @export
get_code_tree <- function() {
  r <- cctu_env$code_tree
  class(r) <- c("code_tree", class(r))
  return(r)
}



#' @describeIn get_code_tree reset the internal code_tree object to have no
#' entries
#' @param root_file the name of the code file you want to use as the initial
#' root for code tree
#' @export
reset_code_tree <- function(root_file = "main.R") {
  cctu_env$code_tree <- data.frame(
    parent = as.character(NULL),
    child = as.character(NULL),
    stringsAsFactors = FALSE
  )
  cctu_env$parent <- root_file
}


#' Print/plot the code tree.
#' @param x code tree object from \code{\link{get_code_tree}}.
#' @inheritParams reset_code_tree
#' @param ... other parameters to be passed to \code{\link[igraph]{plot.igraph}}
#'@examples
#'\dontrun{
#' plot(get_code_tree(), root_file = "main.R")
#'}
#' @export
plot.code_tree <- function(x,
                           root_file = "main.R",
                           ...){

  x[] <- lapply(x,
                gsub,
                pattern = "\\\\",
                replacement = "/")

  com_prefix <- find_common_prefix(x$parent)

  root_file <- paste0(com_prefix, "ROOT")
  x$parent <- gsub("ROOT", root_file, x$parent)
  x$parent <- ifelse(x$parent == root_file,
                                 x$parent,
                                 gsub(com_prefix, "", x$parent))
  x$child <- ifelse(x$child == root_file,
                                x$child,
                                gsub(com_prefix, "", x$child))

  x$parent[x$parent == "ROOT"] <- com_prefix

  g <- igraph::graph_from_data_frame(x, directed=TRUE)

  igraph::plot.igraph(g,
                      layout = igraph::layout_as_tree(g, root = 1),
                      vertex.shape="none",
                      edge.arrow.size = 0.5,
                      margin = 0,
                      asp = 0)
}

#' Print methods for code_tree object
#' @inheritParams plot.code_tree
#' @export
print.code_tree <- plot.code_tree


# Find common prefix
#' @keywords internal
find_common_prefix <- function(vec) {
  if (length(vec) == 0) return("")  # Handle empty input

  # Find the shortest string (common prefix can't be longer than this)
  min_length <- min(nchar(vec))

  # Compare characters across all strings
  for (i in 1:min_length) {
    chars <- unique(substr(vec, i, i))  # Extract i-th character from all strings
    if (length(chars) > 1) {  # If there's more than one unique character, stop
      return(substr(vec[1], 1, i - 1))
    }
  }

  return(substr(vec[1], 1, min_length))  # Entire shortest string is a common prefix
}

