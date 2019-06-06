#library(cct
context("Test clean_meta_table")


X <- cctu::meta_table
test_that("basic and errors",{
  expect_is(cctu:::clean_meta_table(cctu::meta_table), "data.frame")
  expect_error(cctu:::clean_meta_table(X[,-4]))
  expect_warning(cctu:::clean_meta_table(X[,-8]))
  Y <- X
  Y$item <- "shoe"
  expect_warning(cctu:::clean_meta_table(Y))
  Y <- X
  Y$orientation <- "bi"
  expect_warning(cctu:::clean_meta_table(Y))
}
)

test_that("errors in add_program",{

X <- cctu::meta_table
Y <- X[,-4]
assign("meta_table", Y, cctu:::cctu_env)
expect_error( cctu:::add_program("1.1", "test.R"))

Y <- X[,-7]
assign("meta_table", Y, cctu:::cctu_env)
expect_warning(cctu:::add_program("1.1", "test.R"))
})
