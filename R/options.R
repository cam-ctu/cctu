
#' Initialise cctu package environment and options
#'
#' This function will reset the default options. Internal use, user shouldn't call
#' this function during the analysis.
#'
#' @keywords internal
init_cctu_env <- function(){

  # Clean-up everything in the environment
  rm(list = ls(all.names = TRUE, envir = cctu_env), envir = cctu_env)

  # Internal option only
  cctu_env$number <- "0"
  cctu_env$sumby_count <- 0
  cctu_env$nested_run_batch <- FALSE

  # Missing data report
  cctu_env$missing_report_data <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                                          c("form", "visit_var", "visit_label",
                                            "visit", "variable", "label", "missing_pct",
                                            "subject_ID"))
  # Setup DLU file
  cctu_env$dlu <- NULL

  # Set default digital decimal for numeric variables
  cctu_env$digits <- 3

  # Set default digital decimal for percentage
  cctu_env$digits_pct <- 1

  # Set default rounding function
  cctu_env$rounding_fn <- signif_pad

  # Set default subject string
  cctu_env$subjid_string <- "subjid"

  # Set default print plot
  cctu_env$print_plot <- TRUE

}

#' Set cctu package options
#'
#' Allows to set and examine the global options which will affect the behaviors
#' of the \code{cctu} package.
#'
#' @param ... any options can be defined, using \code{name = value}, for example
#' \code{cctu_options(digits = 3)}.
#'
#' @details
#' Invoking \code{cctu_options()} with no arguments returns a list with the
#' current values of the options. Note that not all options listed below are set
#'  initially. To access the value of a single option, one should use, e.g.,
#'  \code{print(cctu_options("digits"))}. You need to use \code{print} in order
#'  to see the value.
#'
#' @section Options used in \code{cctu} package:
#'
#' \describe{
#'   \item{\code{print_plot}}{Logical, used by \code{\link{cttab}} function, if the summary
#'   plot will be printed.}
#'   \item{\code{rounding_fn}}{Name of the rounding function to used by \code{\link{cttab}}
#'    function. Default is \code{\link{signif_pad}}. See \code{\link{cttab}} for details.}
#'   \item{\code{digits_pct}}{A numeric value specifying the number of digits after
#'   the decimal place for percentages, default is \code{1}. Used by \code{\link{cttab}}
#'   function. See \code{\link{cttab}} for details.}
#'   \item{\code{digits}}{A numeric value of rounding to be passed to \code{rounding_fn},
#'    default is \code{3}. Mainly used by \code{\link{cttab}} function. See \code{\link{cttab}}
#'    for details.}
#'   \item{\code{subjid_string}}{A character string indicating the variable name of
#'   the subject ID in the dataset, default is \code{"subjid"}}
#'
#' }
#'
#' @examples
#' \dontrun{
#' # No digits after decmial for percentage
#' cctu_options("digits_pct" = 0)
#' # Initialise the cctu settings.
#' init_cctu_env()
#' }
#'
#' @export

cctu_options <- function(...) {
  # Set/get options in cctu
  # Args: any options can be defined, using 'name = value'
  #
  e <- cctu_env

  # Change to these options will be ignored
  internal_options <- c("sumby_count", "number", "nested_run_batch",
                        "missing_report_data", "dlu")

  a <-  list(...)
  len <- length(a)

  if (len < 1)
    return(as.list(cctu_env))

  a_names <- names(a)

  if (is.null(a_names)) { # case like cctu_options("a", "b")
    empty <- rep(TRUE, len)
    empty_len <- len
  } else { # case like cctu_options(a = 3, b = 4, "c")
    empty <- (a_names == '')
    empty_len <- sum(empty)
  }
  for (i in which(empty)) {
    if (!is.character(a[[i]]))
      stop("cctu_options only accepts arguments as 'name=value' or 'name'")
  }

  r <- if (empty_len < len) mget(a_names[!empty], envir = e, ifnotfound = NA)

  if (empty_len > 0)
    r <- c(r, mget(unlist(a[empty]), envir = e,
                   ifnotfound = list(function(x) {
                     warning("cctu option '", x, "' not found"); NA
                     })))

  # set options
  for (n in a_names[!empty]) {
    if (n %in% internal_options)
      next
    assign(n, a[[n]], e)
  }

  if (len == 1)
    return(invisible(r[[1]]))

  invisible(r)
}


#.reserved <- character(0)

