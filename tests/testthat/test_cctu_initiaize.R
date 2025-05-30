rm(list = ls())
context("cctu_initialise")
library(cctu)
library(testthat)


# rm_output  a) test it

test_that("rm_output", {
  dir.create("Output")
  dir.create(file.path("Output", "Core"))
  write.csv(cctu::meta_table_example, file = file.path("Output", "Core", "meta.csv"))
  expect_gt(length(list.files("Output", recursive = TRUE)), 0)
  rm_output()
  print(list.files("Output", recursive = TRUE))
  expect_equal(length(list.files("Output", recursive = TRUE)), 0)
})

# make sure there is no directory
unlink("Output", recursive = TRUE, force = TRUE)
# system("rm -r Output")
print(list.files())
# check_directory  on empty output

test_that("check_dir negative", {
  expect_warning(
    cctu_check_dir(warnings = TRUE),
    "Default directories needed by cctu do not exist"
  )
  expect_false(cctu_check_dir())
})

# cctu_initialise to create
cctu_initialise()
# check_directory  on fresh output

test_that("check_dir positive", {
  expect_true(cctu_check_dir())
})

# cctu_initialize for scripts,

test_that("check_dir scripts", {
  unlink("main.R")
  unlink("Progs", recursive = TRUE)
  unlink("library", recursive = TRUE)
  cctu_initialize(scripts = TRUE)
  file_list <- list.files(getwd())
  print(system.file(file.path("scripts", "main.R"), package = "cctu"))
  print(file_list)
  expect_true(any(grepl("main.R", file_list)))
  expect_true(any(grepl("Progs", file_list)))
  expect_true(any(grepl("library", file_list)))
  # tidy up
  unlink("main.R")
  unlink("Progs", recursive = TRUE)
  unlink("library", recursive = TRUE)
})

test_that("non root=getwd()", {
  dir.create("nonroot")
  cctu_initialise(root = "nonroot")
  print(list.files())
  file_list <- list.dirs("nonroot", recursive = TRUE)
  print(file_list)
  expect_true(any(grepl("Core", file_list)))
  expect_true(any(grepl("Figures", file_list)))
  expect_true(any(grepl("Reports", file_list)))
  unlink("nonroot", recursive = TRUE)
})

test_that("add spurious files and check rm_output option in cctu_initialise", {
  dir.create("Output")
  dir.create(file.path("Output", "Core"))
  write.csv(cctu::meta_table_example, file = file.path("Output", "Core", "meta.csv"))
  write.csv(cctu::meta_table_example, file = file.path("Output", "meta.csv"))
  cctu_initialise(rm = TRUE)
  expect_false(file.exists(file.path("Output", "Core", "meta.csv")))
  expect_false(file.exists(file.path("Output", "meta.csv")))
})
