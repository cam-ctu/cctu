utils::globalVariables(".")
# may well have to add "cctu_env" to the line above. But I can't get devtools::check() to work past installing,
#so impossible to check at the moment.
cctu_env <- new.env(parent= emptyenv())
cctu_env$number <- "0"
cctu_env$sumby_count <- 0


#' @export
reset_code_tree <- function(root="main.R"){
  cctu_env$code_tree <- data.frame(parent = as.character(NULL),
                                   child = as.character(NULL),
                                   stringsAsFactors = FALSE)
  cctu_env$parent <- root
}
