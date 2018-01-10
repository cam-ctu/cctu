#### Tidies up names of a data frame within certain rules 
PROGNAME <- "clean_names.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 
#### Notes: 
#### - if convert = TRUE, actively modifies df in parent environment,
####   else returns modified df
#### - variable names converted to lower case, "." replaced with "_"


clean_names <- function(df, convert = TRUE){
  arg_name_df <- deparse(substitute(df))
  df_names    <- names(df)           %>%
                 tolower(.)          %>%
                 gsub("\\s+","_",.)  %>%
                 gsub("/","_",.)     %>%  
                 gsub("\\.", "_", .) %>%
                 gsub("_+", "_", .)  %>%
                 gsub("^_", "", .)   %>%
                 gsub("_$", "", .)
  if(convert){
    names(df) <- df_names
    assign(arg_name_df, df, envir = parent.frame())
  } else{
    df_names
  }
}
