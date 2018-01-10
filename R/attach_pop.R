#### Function that uses attach() to switch between different sets of data.frames
#### based on looking up from table no. in TableofTables - see population.R code
PROGNAME <- "attach_pop.R"
#### Author: Simon Bond
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 24MAY2017
#### Notes: 


# attach population
attach_pop <- function(number 
                       #TableofTables=TableofTables, 
                       #population_title=population_title
                       ){
  table_of_tables <- merge(TableofTables, population_title, 
                           by.x = "Population", by.y = "title", all.x = TRUE)
  pop_name        <- as.character(subset(table_of_tables, Number == number, select = "pop_name"))
  eval(call("attach", as.name(pop_name)))
}

# detach population
# this will be used inside writeTable(), writeGgplot()
detach_pop <- function(number 
                       #TableofTables=TableofTables, 
                       #population_title=population_title
                      ){
  table_of_tables <- merge(TableofTables, population_title, 
                           by.x = "Population", by.y = "title", all.x = TRUE)
  pop_name <- as.character(subset(table_of_tables, Number == number, select = "pop_name"))
  eval(call("detach", as.name(pop_name)))
}


clean_up <- function(number, envir = parent.frame()){
  obj_list <- ls(envir)
  if(!exists("RESERVED", envir = envir)){RESERVED <- NULL}
  # belt and braces - try to define RESERVED elsewhere to contain an element with value "RESERVED"
  keep     <- match(c("RESERVED", RESERVED), obj_list)
  obj_list <- obj_list[-keep]
  rm(list = obj_list, envir = envir)
  detach_pop(number)
}
