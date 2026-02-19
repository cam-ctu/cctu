options(stringsAsFactors = FALSE)

data <- read.csv(system.file("extdata", "dirtydata.csv", package = "cctu"))
data %<>% clean_names() %>% remove_blank_rows_cols()

codes <- read.csv(system.file("extdata","codes.csv", package="cctu")) %>% clean_names()

#  Use the read_data() functions  and then
# apply_macro_dict()   possibly
#  data_table_summary(data_table)  to give a table of the input data.

for( x in unique(codes$var)){
  code_df <-  subset(codes, var==x)
  print(code_df)
  data[,x] <- factor( data[,x], levels=code_df$code, labels=code_df$label)
}

data$start_date <- as.POSIXct( data$start_date , format="%d/%m/%Y")
data_name <- names(data)
names(data)[match("subject_id", data_name)] <- "subjid"

set_meta_table( cctu::meta_table )

#Create the population table

popn <- data[,"subjid", drop=FALSE]
popn$safety <- TRUE
popn$full <- popn$subjid<5

create_popn_envir("data",popn)

#tidy up
rm(code_df, codes, data_name, x)
.reserved <- ls()


