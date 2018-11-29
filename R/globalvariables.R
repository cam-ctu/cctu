utils::globalVariables(".")
# may well have to add "cctu_env" to the line above. But I can't get devtools::check() to work past installing,
#so impossible to check at the moment.
cctu_env <- new.env(parent= emptyenv())
cctu_env$number <- "0"
cctu_env$sumby_count <- 0

.reserved <- character(0)

