rm(list = ls())
context("propercase")
library(cctu)
library(testthat)


test_that("standard", {
  expect_equal(propercase("heLLo WORLD"), "Hello World")
})

test_that("apostrophe", {
  expect_equal(propercase("Addenbrooke's"), "Addenbrooke's")
})
