#.libPaths(c(.libPaths(), "V:/STATISTICS/STUDY PLANNING/R_library"))
#.libPaths("V:/STATISTICS/STUDY PLANNING/R_library")
# Sys.setenv(R_LIBS_USER="V:/STATISTICS/STUDY PLANNING/R_library")

rm(list=ls())
context("Test the testing")
library(cctu)
library(testthat)
library(readxl)
library(magrittr)

#PATH <- paste0(getwd(),"/tests/testthat/")
#PATH <- paste0(getwd(),"/")
#setwd(PATH)
#meta_table is a special name that is set as default in numerous other functions
meta_table <- read_excel(system.file("extdata", "meta_table.xlsx", package="cctu")) %>% as.data.frame(stringsAsFactors=FALSE)
meta_subset <- meta_table[3,]


set.seed(1649)
data <- data.frame( subjid=1:100,
                    endpoint=rnorm(100) %>% round(2),
                    response=rep(c("Fail","Respond"),rep(50,2)),
                    rx=rep(c("A","B"),50)
                    )
popn <- data.frame(subjid=data$subjid,
                   safety=rep(TRUE,100),
                   full=rep(rep(c(TRUE,FALSE),c(9,1)),10)
)

create_popn_envir(c("data"), popn)

#Don;t actually need PATH defined or included in RESERVED as the current code stands!
RESERVED <- c("meta_table","meta_subset","popn","safety","full")
source("analysis.R", local=TRUE)

#print(code_tree)

test_that("exist",
          {
            expect_true(exists("popn"))
            expect_equal(get_obj("popn"),popn)
          }
          )

#create the correct population labels
# need to check the order of unique(meta_table$Population)

popn_size <- apply(popn[,names(popn)!="subjid"],2,sum)
popn_labels <- paste0(c("Safety (N = ","Full Analysis (N = "), popn_size,c(")", ")"))

#setwd(PATH)
create_word_xml("Test Report",
                "Simon Bond",
                meta_table,
                datestamp="Test Date",
                popn_labels = popn_labels)
create_word_xml("Test Report Jpeg",
                "Simon Bond",
                meta_subset,
                datestamp="Test Date",
                filename="Output\\Reports\\ReportJpg.doc",
                figure_format="jpeg",
                popn_labels = popn_labels)

#out to write.csv(meta_table) to record the final version post code.

test_that("Creation of files",{
  expect_true(file.exists("Output/Reports/Report.doc"))
  expect_true(file.exists("Output/Reports/ReportJpg.doc"))
  expect_true(file.exists("Output/Core/table_1.1.xml"))
  expect_true(file.exists("Output/Core/table_1.2.xml"))
  expect_true(file.exists("Output/Figures/fig_1.3.png"))
  expect_true(file.exists("Output/Figures/fig_1.3.jpeg"))
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


