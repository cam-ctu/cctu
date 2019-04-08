rm(list=ls())

library(cctu)
#library(testthat)
#library(magrittr)
#library(dplyr)
context("Test get_obj")

test_that("basic",
  {
    x <- 1
    y <- cctu:::get_obj("x")
    expect_equal(y,x)

  }
)

test_that("not found",
          {

            expect_warning(cctu:::get_obj("x"),"x not found")
        }
)

test_that("environ",
          {
            x <- 1
            tmp_env <- new.env(parent = .GlobalEnv)
            #will not find x as test_that creates a different envir, not the .Global.
            tmp_env2 <- new.env()
            y <- cctu:::get_obj("x", frame=tmp_env2)
            expect_equal(y,x)
            expect_warning(cctu:::get_obj("x", frame=tmp_env),"x not found")
          }
)
