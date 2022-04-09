
utils::globalVariables(".")

cctu_env <- new.env(parent= emptyenv())

.onLoad <- function(...) {
  init_cctu_env()
}


