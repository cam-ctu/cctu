# Need to add more

test_that("Test for signif_pad", {
  x <- c(0.9001, 12356, 1.2, 1., 0.1, 0.00001 , 1e5, 1.3467)
  expect_identical(signif_pad(x, digits=3),
                   c("0.900", "12400", "1.20", "1.00", "0.100",
                     "0.0000100", "100000", "1.35"))

  expect_identical(signif_pad(x, digits=3, round.integers=FALSE),
                   c("0.900", "12356", "1.20", "1.00", "0.100",
                     "0.0000100", "100000", "1.35"))

})

test_that("Test for round_pad", {
  x <- c(0.9001, 12356, 1.2, 1., 0.1, 0.00001 , 1e5, 1.3467)
  expect_identical(round_pad(x, digits = 2),
                   c("0.90", "12356.00", "1.20", "1.00", "0.10",
                     "0.00", "100000.00", "1.35"))

  x <- c(0, 0.9001, 156, 1.2, 1., 0.1, 0.00001 , 0.00007, 0.0003)
  expect_identical(format_percent(x, digits=2),
                   c("0%", "90.01%", "15600.00%", "120.00%", "100%",
                     "10.00%", "0.00%", "0.01%", "0.03%"))

  x <- c(NaN, 0.4, NA, 1)
  expect_equal(format_percent(x, digits=2),
               c("", "40.00%", "", "100%"))

  x <- c(1, 2, 3, "a")
  expect_error(format_percent(x, digits=2),
               "x must be numeric.")


})

test_that("Test for p-value", {
  pv <- c(-1, 0.00001, 0.00093, 0.001, 0.0042, 0.8999, 0.9, 1, NA)
  expect_identical(format_pval(pv),
                   c("<0.001", "<0.001", "<0.001", "0.001",  "0.004",
                     "0.900", "0.900", "1.000", NA))


})



