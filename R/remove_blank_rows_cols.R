#' Function to remove blank rows/columns from a df
#'
#' @param df a data.frame
#' @param convert a logical to indicate if you want to modify \code{df} in the parent environment,
#' or if not simply return a modified version of \code{df}
#' @param verbose logical to print information on changes to the global environment or external files. Defaults to options()$verbose.
#' @param which one of \code{"rows"}, \code{"cols"}, or \code{"both"}. Default is \code{"both"} to removing both empty rows and empty columns.
#'
#' @return this removes columns and rows that are totally empty (either "" or NA).
#' @export



remove_blank_rows_cols <- function(df, convert = TRUE, verbose=options()$verbose,
                                   which = c("both", "rows", "cols")){

  which <- match.arg(which)
  arg_name_df <- deparse(substitute(df))
  # Remove rows
  if(which %in% c("both", "rows")){
    df          <- df[Reduce(`|`, lapply(df, function(x) !is_empty(x))),]
  }
  # Remove cols
  if(which %in% c("both", "cols")){
    df          <- Filter(function(x) !all(is_empty(x)), df)
  }
  if(convert){
    assign(arg_name_df, df, envir = parent.frame())
    if(verbose){message(arg_name_df, " modified with blanks removed.")}
    invisible(df)
  } else {
    df
  }
}
