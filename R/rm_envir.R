#' A function to remove all attached objects from the search path with any
#' exceptions specified
#'
#' @param ignore a character vector of regular expression search terms  within
#'  the output from \code{search()} as to which should be retained.
#' @param verbose logical to print information on changes to the global
#' environment or external files. Defaults to options()$verbose.
#' @inheritParams base::grep
#' @details it detaches anything not ignored. It is called by default within
#'  \code{\link{attach_pop}} .
#' @seealso  \code{\link{search}} \code{\link{attach_pop}}
#' @export



rm_envir <- function(
    ignore = c(
      ".GlobalEnv", "package:", "tools:",
      "Autoloads"
    ),
    verbose = options()$verbose,
    perl = FALSE) {
  base_items <- c(
    ".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
    "package:grDevices", "package:utils", "package:datasets",
    "package:methods", "Autoloads", "package:base"
  )
  searchpath <- search()
  for (pattern in ignore) {
    searchpath <- searchpath[!grepl(pattern, searchpath, perl = perl)]
  }
  for (item in searchpath) {
    if (item %in% base_items) {
      warning(paste0("Removing ", item, " may be disastrous."))
    }
    detach(item, character.only = TRUE)
    if (verbose) {
      cat(item, "detached.\n")
    }
  }
}
