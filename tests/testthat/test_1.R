#.libPaths(c(.libPaths(), "V:/STATISTICS/STUDY PLANNING/R_library"))


context("Test the testing")
library(cctu)
library(testthat)
library(readxl)
library(magrittr)

#PATH <- paste0(getwd(),"/tests/testthat/")
PATH <- paste0(getwd(),"/")
#setwd(PATH)
#meta_table is a special name that is set as default in numerous other functions
meta_table <- read_excel(system.file("extdata", "meta_table.xlsx", package="cctu")) %>% as.data.frame(stringsAsFactors=FALSE)
set.seed(1649)
data <- data.frame( endpoint=rnorm(100) %>% round(2),
                    response=rep(c("Fail","Respond"),rep(50,2)),
                    rx=rep(c("A","B"),50))
safety <- new.env()
assign("data",data, envir=safety)
rm(data)
popn_table <- data.frame(title="Safety",pop_name="safety")
RESERVED <- c("meta_table","PATH","popn_table","safety")
source("analysis.R", echo = FALSE, local=TRUE)

f <- function(){
  if(exists("popn_table")){"yes"
    }else{"no"}
}


get_obj2 <- function(name, alt=NULL){
  if(exists(name)){
    get(name)
  } else{
    warning(paste(name, "not found"))
    alt
  }
}


test_that("exist",
          {
            expect_equal(f(), "yes")
            expect_true(exists("popn_table"))
            expect_equal(get_obj2("popn_table"),popn_table)
            expect_equal(get_obj("popn_table"),popn_table)
          }
          )



create_word_xml("Test Report", "Simon Bond", datestamp="Test Date")
create_word_xml("Test Report Jpeg", "Simon Bond",datestamp="Test Date",filename="Output\\Reports\\ReportJpg.doc", figure_format="jpeg")

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
  # Won;t work given the explicit file path in the footnotes
  #test <- read_xml("Output/Reports/Report.doc")
  #expect_known_output(test %>%as.character, "data/Report", update=FALSE, print=TRUE)
  #test <- read_xml("Output/Reports/ReportJpg.doc")
  #expect_known_output(test %>%as.character, "data/ReportJpg", update=FALSE, print=TRUE)
})





#clean names
#propercase
#rbind space
#remove blank rows cols

#maybe
#srce
#sumby_ae


