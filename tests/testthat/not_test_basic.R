library(cctu)
library(testthat)
library(readxl)
library(magrittr)
context("Basic check of working structure")

test_that("Whatever",
          expect_true(TRUE)
          )
# Note the convention of putting a / at the end of partial file paths, and not at the begining of a suffix
#meta_table is a special name that is set as default in numerous other functions
#PATH <- paste0(getwd(),"/tests/testthat/")
PATH <- paste0(getwd(),"/")
setwd(PATH)
#meta_table is a special name that is set as default in numerous other functions
meta_table <- read_excel(system.file("extdata", "meta_table.xlsx", package="cctu")) %>% as.data.frame(stringsAsFactors=FALSE)

#make data and populations
set.seed(1649)
data <- data.frame( endpoint=rnorm(100) %>% round(2),
                    response=rep(c("Fail","Respond"),rep(50,2)),
                    rx=rep(c("A","B"),50))
safety <- new.env()
assign("data",data, envir=safety)
rm(data)
#popn_table and RESERVED are special names that are set as default in numerous other functions
popn_table <- data.frame(title="Safety",pop_name="safety")
RESERVED <- c("meta_table","PATH","popn_table","safety")
#source some code to make tables and figures
source("analysis.R", echo = TRUE)




#output
create_word_xml("Test Report", "Simon Bond")
create_word_xml("Test Report Jpeg", "Simon Bond",filename="Output\\Reports\\ReportJpg.doc", figure_format="jpeg")

test_that("Creation of files",{
  expect_true(file.exists("Output/Reports/Report.doc"))
  expect_true(file.exists("Output/Reports/ReportJpg.doc"))
  expect_true(file.exists("Output/Core/table_1.1.xml"))
  expect_true(file.exists("Output/Figures/fig_1.2.png"))
  expect_true(file.exists("Output/Figures/fig_1.2.jpeg"))
})



test_that("Comparison to saved output",{
library(xml2)
test <- read_xml("Output/Core/table_1.1.xml")
expect_known_output(test %>%as.character, "data/table1.1", update=FALSE, print=TRUE)
test <- read_xml("Output/Reports/Report.doc")
#expect_known_output(test %>%as.character, "data/Report", update=FALSE, print=TRUE)
test <- read_xml("Output/Reports/ReportJpg.doc")
#expect_known_output(test %>%as.character, "data/ReportJpg", update=FALSE, print=TRUE)
})








#clean names
#propercase
#rbind space
#remove blank rows cols

#maybe
#srce
#sumby_ae





