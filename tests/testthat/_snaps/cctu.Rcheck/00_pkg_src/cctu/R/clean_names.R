#' Tidies up names of a data frame within certain rules
#'
#' @param df a data frame
#' @param convert a logical to indicate if you want to modify \code{df} in the
#' parent environment,
#' or if not simply return a character vector of the revised variable names.
#' @param verbose logical to print information on changes to the global
#' environment or external files. Defaults to options()$verbose.
#'
#' @return \code{df} is modified to have variable names that are
#' \itemize{
#'   \item lower case
#'   \item dots, slashes,+ replaced with _
#'   \item no leading or trailing whitespace
#' }
#' so a variable called "Nasty/Var+Name " gets turned into "nasty_var_name" .
#'
#' \code{clean_names} returns either a vector of cleaned names is return
#' directly, or the input object is modified in the parent envirnoment
#' (the default) and an invisible copy of the modified input returned
#'
#'
#' @export
#' @importFrom  magrittr %>%


clean_names <- function(df, convert = TRUE, verbose = options()$verbose) {
  arg_name_df <- deparse(substitute(df))
  df_names <- clean_string(names(df))

  if (convert) {
    names(df) <- df_names
    assign(arg_name_df, df, envir = parent.frame())
    if (verbose) {
      cat(arg_name_df, "given clean names")
    }
    invisible(df)
  } else {
    df_names
  }
}

#'
#' @rdname clean_names
#' @export
#'
#' @param x a character string to be cleaned.
#' @returns \code{clean_string} returns an individual vector with the contents
#' cleaned.
#'
clean_string <- function(x) {
  x %>%
    tolower(.) %>%
    gsub("\\s+", "_", .) %>%
    gsub("-", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("\\.", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_", "", .) %>%
    gsub("_$", "", .) %>%
    trimws()
}
