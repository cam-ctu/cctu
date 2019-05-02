rm(list=ls())
context("cctu_initialise")
library(cctu)
library(testthat)


# rm_output  a) test it

test_that( "rm_output",
           {
             dir.create("Output")
             dir.create("Output/Core")
             write.csv(cctu::meta_table, file="Output/Core/meta.csv")
             expect_gt(length(list.files("Output", recursive=TRUE)), 0)
             rm_output()
             expect_equal(length(list.files("Output", recursive=TRUE)), 0)

           }

)

# make sure there is no directory
unlink("Output", recursive=TRUE)

# check_directory  on empty output

test_that("check_dir negative",{
expect_warning(cctu_check_dir(warnings=TRUE),
               "Default directories needed by cctu do not exist")
expect_false( cctu_check_dir())

}
)

# cctu_initialise to create
cctu_initialise()
# check_directory  on fresh output

test_that("check_dir positive",{
   expect_true( cctu_check_dir())

}
)

# cctu_initialize for scripts,

test_that("check_dir scripts",{
  unlink("main.R")
  unlink("Progs", recursive = TRUE)
  unlink("library", recursive = TRUE)
  cctu_initialize(scripts=TRUE)
  file_list <- list.files(getwd())
  expect_true(any( grepl("main.R" , file_list)))
  expect_true(any(grepl("Progs" , file_list)))
  expect_true(any(grepl("library" ,file_list)))
  #tidy up
  unlink("main.R")
  unlink("Progs", recursive = TRUE)
  unlink("library", recursive = TRUE)

}
)

test_that("non root=getwd()",
          {
              dir.create("nonroot")
              cctu_initialise(root="nonroot")
              file_list <- list.files("nonroot", recursive = TRUE, include.dirs = TRUE)
              expect_true( "Output/Core" %in% file_list )
              expect_true( "Output/Figures" %in% file_list )
              expect_true( "Output/Reports" %in% file_list )
              unlink("nonroot", recursive=TRUE)
          }

)

test_that( "add spurious files and check rm_output option in cctu_initialise",
           {
             dir.create("Output")
             dir.create("Output/Core")
             write.csv(cctu::meta_table, file="Output/Core/meta.csv")
             cctu_initialise(rm=TRUE)
             expect_false(file.exists("Output/Core/meta.csv"))

           }

)


