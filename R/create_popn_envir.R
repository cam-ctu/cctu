#'create environments contain a set of data frames filtered for each population
#'
#'@param data_names a character vector of the key data frames to be filtered
#'@param popn_table_string the character name of the data frame connect the population environment names to be created and the names used the meta_table
#'@param popn_data_string the chacter name of a data frame one-row per partipant  a subjid varaibel and a column for each population indicating if a participant belongs to the population
#'@param subjid_string the character naming the column used in each data frame to identify the subjid. Must be constant and also used in popn_data.
#'
#'
#'@details  Now to use the data frames defined above in data_names you have to either directly
#'call attach(safety), for example orr lookup with attach_pop(1.01) for a table number.
#'There are separate R environments made for each of the populations in population & population_title
#'be careful to use detach(safety) - this is automated with writeTable() and writeGGplot()
#'


  create_popn_envir_function( data_names,
                              popn_table_string="popn_table",
                              popn_data_string="popn_data",
                              subjid_string="subjid",
                              frame=parent.env()
  ){

   popn_table <- get_obj(popn_table_string, frame=frame)
   popn_data <- get_obj(popn_data_string, frame=frame)

for(row in 1:nrow(popn_table)){
  env_temp <- new.env(parent=frame)
  # work through the columns of population, and create environments for all
  for(df in data_names){
    data_temp <- eval(as.name(df), envir=parent)
    # important here to automate - depends on consistent use of subjid
    # some data frames might not have subjid ...
    popn_temp        <- popn_data[, c(subjid_string, popn_table[row, "pop_name"])]
    names(population_temp) <- c(subjid_string, "filter")
    # take a subset based on the logical variable in population
    data_temp <- merge(data_temp, population_temp, by = subjid_string)
    data_temp <- subset(data_temp, filter, select = (names(data_temp) != "filter"))
    assign(df, data_temp, envir = env_temp)
  }
  # keep an environment of the different population-based version of the data sets
  assign(popn_table[row, "pop_name"], env_temp, envir=frame)
 #rm(env_temp, data_temp, population_temp)
}

# remove the original data frames from the search path - else you will get duplications
rm(list = data_names, envir=frame)
#rm(data_names, row, df)

}
