#### Function to remove blank rows/columns from a df 
PROGNAME <- "remove_blank_rows_cols.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 
#### Notes: 
#### - if convert = TRUE, actively modifies df in parent environment, else
####   returns modified df


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