context("Test p_format")


test_that("regular usage", {
  p <- p_format(c(0.000001, 0.451234))
  expect_equivalent(p, c("<0.001", " 0.451"))
})

test_that("error input", {
  expect_error(p_format(factor(0.2)), "'p_format' requires numeric input")
  expect_error(p_format(0.2, digits = 1.4), "'digits' argument must be an integer")
})

test_that("warnings", {
  expect_warning(p_format(-1), "p\\-values outside of the \\[0,1\\] range")
  expect_warning(p_format(2), "p\\-values outside of the \\[0,1\\] range")
  expect_warning(p_format(0.1, digits = 0), "ouput is rounding to the left of the decimal point")
})
