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
              cmd <- paste0( R.home("bin"), '/R')# --ess --no-save')
              args <- c("--ess","--no-save")
            } else{
              cmd <- paste0( R.home("bin"), '/R')
              args <- c("--interactive", "--no-save")
            }
            print(getwd())
            ## THis is where it fails,

            ## It tries to use the UNC path as the working directoy,
            ## But this fails and it defaults C:/Windows.
            ## Need to find another way to simulate interactive mode

            system2(cmd, args=args, input=readLines("container.R"))
            #shell("R --ess --vanilla < container.R ")
            print(getwd())
            expect_equal(file.exists("batch_test.csv"), TRUE)
          }
)
