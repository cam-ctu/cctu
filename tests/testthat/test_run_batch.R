rm(list=ls())
context("run_batch")
#library(cctu)
library(testthat)


test_that("basic test",
          {
            expect_warning(run_batch("script_to_test_run_batch.R"),
                           "run_batch\\(\\) only works in interactive mode")
            file.remove("batch_test.csv")
            expect_equal(file.exists("batch_test.csv"), FALSE)
            print(Sys.info()["sysname"])
            #need this to fool it into being interactive
            if(grepl("[Ww]indows",Sys.info()["sysname"]) ){
              cmd <- paste0( R.home("bin"), '/R --ess --no-save')
            } else{
              cmd <- paste0( R.home("bin"), '/R --interactive --no-save')
            }
            system(cmd, input=readLines("container.R"))
            #shell("R --ess --vanilla < container.R ")
            expect_equal(file.exists("batch_test.csv"), TRUE)
          }
)
