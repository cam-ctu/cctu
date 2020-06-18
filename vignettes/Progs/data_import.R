options(stringsAsFactors = FALSE)

data_table <- data.frame(
  name=c("data","codes"),
  file=c("dirtydata.csv",
         "codes.csv"),
  folder=system.file("extdata", package="cctu"),
  stringsAsFactors = FALSE
)


read_data(data_table)
# if you wanted to read in just one data set, using non-standard options say
read_data( data_table[2,], remove_blank_rows_cols_option=FALSE, clean_names_option=FALSE)


set_meta_table( cctu::meta_table_example)
write_table(data_table_summary(data_table),number = "9", clean_up = FALSE)


# Not strictly needed as the default is to apply these two functions inside of read_data(), but this
# can be turned off with clean_names_option = FALSE , or remove_blank_rows_cols_option=FALSE
data %<>% clean_names() %>% remove_blank_rows_cols()
codes %<>% clean_names


for( x in unique(codes$var)){
  code_df <-  subset(codes, var==x)
  print(code_df)
  data[,x] <- factor( data[,x], levels=code_df$code, labels=code_df$label)
}

data$start_date <- as.POSIXct( data$start_date , format="%d/%m/%Y")
data_name <- names(data)
names(data)[match("subject_id", data_name)] <- "subjid"



#Create the population table

popn <- data[,"subjid", drop=FALSE]
popn$safety <- TRUE
popn$full <- popn$subjid<5

create_popn_envir("data",popn)

#tidy up
rm(code_df, codes, data_name, x)
.reserved <- ls()


