.libPaths("V:/STATISTICS/STUDY PLANNING/R_library")
library(cctu)
library(readxl)
library(magrittr)
context("Basic check of working structure")

PATH <- paste0(getwd(),"/tests/testthat/")
setwd(PATH)
meta_table <- read_excel(system.file("extdata", "meta_table.xlsx", package="cctu")) %>% as.data.frame(stringsAsFactors=FALSE)

#make data and populations
set.seed(1649)
data <- data.frame( endpoint=rnorm(100) %>% round(2),
                    response=rep(c("Fail","Respond"),rep(50,2)),
                    rx=rep(c("A","B"),50))
safety <- new.env()
assign("data",data, envir=safety)
rm(data)
popn_table <- data.frame(title="Safety",pop_name="safety")
RESERVED <- c("meta_table","PATH","popn_table","safety")
#source some code to make tables
detach()
detach()
source("analysis.R", echo = TRUE)

#output




