
test_that("library_description", {
  dir <- tempdir()
  withr::defer(fs::dir_delete(dir))
  usethis::create_project(dir)
  usethis::local_project(path = dir)
  expect_warning( cctu::library_description(),
                  "No DESCRIPTION file so no packages loaded"
                  )
  cctu::cctu_initialise(description = TRUE)
  expect_contains(cctu::library_description(),"tidyverse")
  expect_contains(search(),"package:eudract")
})
