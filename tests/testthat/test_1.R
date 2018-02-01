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
#source("analysis.R", echo = TRUE)

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


#cctu:::.eval_pop(1.1, "attach")
attach_pop(1.1)
#X <- sumby(endpoint, rx, data=data )
#write_table(1.1,X)

get_obj("popn_table")
test_that("arithmetic",
  expect_equal(1+1,2)
)

