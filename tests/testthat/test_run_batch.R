rm(list=ls())
context("run_batch")
library(cctu)
library(testthat)


test_that("basic Windows",
          {
            skip_if(Sys.info()["sysname"] != "Windows", "run_batch only works on Windows")
            expect_warning(run_batch("script_to_test_run_batch.R"),
                           "run_batch\\(\\) only works in interactive mode")
            file.remove("batch_test.csv")
            expect_equal(file.exists("batch_test.csv"), FALSE)
            #need this to fool it into being interactive
            shell("Rterm.exe --ess --vanilla < container.R ")
            expect_equal(file.exists("batch_test.csv"), TRUE)

          }
)

test_that("non windows",
          {
            skip_if(Sys.info()["sysname"] == "Windows", "Only run for non-windows OS")
            expect_warning(run_batch("script_to_test_run_batch.R"),
                           "run_batch\\(\\) only works in interactive mode")
            return_text <- system("$(R_HOME)/bin/R --interactive < container.R ", intern=TRUE)
            expect_match(paste(return_text,collapse="\n"),"run_batch\\(\\) only works in Windows")
          }
)



