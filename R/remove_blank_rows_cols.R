#' Function to remove blank rows/columns from a df
#'
#' @param df a data.frame
#' @param convert a logical to indicate if you want to modify \code{df} in the parent environment,
#' or if not simply return a modified version of \code{df}
#' @param verbose logical to print information on changes to the global environment or external files. Defaults to options()$verbose.
#' @param rows logical whether to drop blank rows. Default TRUE.
#' @param cols logical whether to drop blank columns. Default TRUE.
#'
#' @return this removes columns and rows that are totally empty (either "" or NA).
#' @export



remove_blank_rows_cols <- function(df, convert = TRUE, verbose=options()$verbose,
                                   rows=TRUE, cols=TRUE){
  if(!rows & !cols ){warning("nothing will happen with both rows and cols set to FALSE")}
  arg_name_df <- deparse(substitute(df))
  check       <- function(x){all(is.na(x) | (x == ""))}
  if(rows){
    drop        <- apply(df, 1, check)
    df          <- df[!drop, ]
  }
  if(cols){
    drop        <- apply(df, 2, check)
    df          <- df[, !drop]
  }
  if(convert){
    assign(arg_name_df, df, envir = parent.frame())
    if(verbose){message(arg_name_df, " modified with blanks removed.")}
    invisible(df)
  } else {
    df
  }
}
