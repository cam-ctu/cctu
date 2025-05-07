
test_that("cctu_initialise warning", {
  # dir <- tempdir()
  # withr::defer(fs::dir_delete(dir))
  # usethis::create_project(dir)
  # usethis::local_project(path = dir)
#  options(verbose = TRUE)
  local_create_project()
  expect_warning(cctu::cctu_initialise(description = FALSE),
                 #"test the warning"#
                 "Recommend to create and edit a DESCRIPTION file"
                 )
  expect_false(file.exists("DESCRIPTION"))
  cctu::cctu_initialise(description = TRUE)
  expect_true(file.exists("DESCRIPTION"))
})


# can't make this work.  Need to rely on test_1 and ensure this runs first.
# In turn this needs the underscoring in the file names to be respected.

# test_that("source warning", {
#   dir <- tempdir()
#   withr::defer(fs::dir_delete(dir))
#   usethis::create_project(dir)
#   usethis::local_project(path = dir)
#   #  options(verbose = TRUE)
#   expect_warning(
#cctu::source(
#     system.file("tests/testthat/hello_world.R", package = "cctu")
#   )
#,
#                  #"test the warning"#
#                  "Recommend using cctu_initialise()"
#   )
# })
