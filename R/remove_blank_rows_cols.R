#' Function to remove blank rows/columns from a df
#'
#' @param df a data.frame
#' @param convert a logical to indicate if you want to modify \code{df} in the parent environment,
#' or if not simply return a modified version of \code{df}
#'
#' @return this removes columns and rows that are totally empty (either "" or NA).
#' @export



remove_blank_rows_cols <- function(df, convert = TRUE){
  arg_name_df <- deparse(substitute(df))
  check       <- function(x){all(is.na(x) | (x == ""))}
  drop        <- apply(df, 1, check)
  df          <- df[!drop, ]
  drop        <- apply(df, 2, check)
  df          <- df[, !drop]
  if(convert){
    assign(arg_name_df, df, envir = parent.frame())
  } else {
    df
  }
}
