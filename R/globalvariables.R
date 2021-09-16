utils::globalVariables(c("."))
# may well have to add "cctu_env" to the line above. But I can't get devtools::check() to work past installing,
#so impossible to check at the moment.
cctu_env <- new.env(parent= emptyenv())
cctu_env$number <- "0"
cctu_env$sumby_count <- 0
cctu_env$nested_run_batch <- FALSE

#.reserved <- character(0)

