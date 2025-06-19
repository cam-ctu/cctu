# library(cct
context("Test clean_meta_table")


X <- cctu::meta_table_example
test_that("basic and errors", {
  expect_is(cctu:::clean_meta_table(cctu::meta_table_example), "data.frame")
  expect_error(cctu:::clean_meta_table(X[, -4]))
  expect_warning(cctu:::clean_meta_table(X[, -8]))
  Y <- X
  Y$item <- "shoe"
  expect_warning(cctu:::clean_meta_table(Y))
  Y <- X
  Y$orientation <- "bi"
  expect_warning(cctu:::clean_meta_table(Y))
  # check auto adding of orientation
  Y2 <- Y[, names(Y) != "orientation"]
  Y2 <- cctu:::clean_meta_table(Y2)
  expect_true("orientation" %in% names(Y2))
  expect_true(all(Y2$orientation == "portrait"))
})

test_that("errors in add_program", {
  X <- cctu::meta_table_example
  Y <- X[, -4]
  assign("meta_table", Y, cctu:::cctu_env)
  expect_error(cctu:::add_program("1.1", "test.R"))

  Y <- X[, -7]
  assign("meta_table", Y, cctu:::cctu_env)
  expect_warning(cctu:::add_program("1.1", "test.R"))
})
