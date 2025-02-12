rm(list = ls())

library(cctu)

context("Test rm_envir")

test_that("Basic", {
  newenvir <- new.env()
  attach(newenvir)
  expect_true("newenvir" %in% search())
  rm_envir()
  expect_false("newenvir" %in% search())
})

test_that("warning", {
  expect_warning(rm_envir(ignore = c(".GlobalEnv", "package:(?!datasets)", "tools:", "Autoloads"), perl = TRUE))
  expect_false("package:datasets" %in% search())
  library(datasets)
  expect_true("package:datasets" %in% search())
})
