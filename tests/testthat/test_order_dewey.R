context("test order_dewey")

test_that("basic ordering", {
  number <- c("2", "1.0", "0.2.3", "10.10", "10.9")
  x <- order_dewey(number)
  number[x]
  expect_equal(x, c(3, 2, 1, 5, 4))
  expect_equal(order_dewey(as.character(1:4)), 1:4)
})
test_that("error", {
  expect_error(order_dewey(1:2), "Must input a character vector")
  expect_error(order_dewey(c("a", "b")), "non numeric values are not allowed")
})
