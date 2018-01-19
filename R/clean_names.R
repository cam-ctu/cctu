#' Tidies up names of a data frame within certain rules
#'
#' @param df a data frame
#' @param convert a logical to indicate if you want to modify \code{df} in the parent environment,
#' or if not simply return a character vector of the revised variable names.
#'
#' @return \code{df} is modified to have variable names that are
#' \itemize{
#'   \item lower case
#'   \item dots, slashes,+ replaced with _
#'   \item no leading or trailing whitespace
#' }
#' so a variable called "Nasty/Var+Name " gets turned into "nasty_var_name" .
#'
#' Either a vector of cleaned names is return directly,
#' or the input object is modified in the parent envirnoment (the default)
#'
#'
#' @export
#' @importFrom  magrittr %>%


clean_names <- function(df, convert = TRUE){
  arg_name_df <- deparse(substitute(df))
  df_names    <- names(df)           %>%
                 tolower(.)          %>%
                 gsub("\\s+","_",.)  %>%
                 gsub("/","_",.)     %>%
                 gsub("\\.", "_", .) %>%
                 gsub("_+", "_", .)  %>%
                 gsub("^_", "", .)   %>%
                 gsub("_$", "", .)   %>%
                 trimws
  if(convert){
    names(df) <- df_names
    assign(arg_name_df, df, envir = parent.frame())
  } else{
    df_names
  }
}
