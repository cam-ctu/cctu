test_that("Test is_empty", {
  expect_false(is_empty("test"))
  expect_true(is_empty(""))
  expect_true(is_empty(NA))
  expect_true(is_empty(NULL))

  # string is not empty
  expect_false(is_empty(" "))
  expect_true(is_empty(trimws(" ")))

  # numeric vector
  x <- 1
  expect_false(is_empty(x))
  expect_true(is_empty(x[-1]))

  # check multiple elements of character vectors
  expect_identical(
    unname(is_empty(c("", "a"))),
    c(T, F)
  )

  expect_identical(
    unname(is_empty(c(NA, "a"))),
    c(T, F)
  )

  # empty data frame and list
  expect_true(is_empty(data.frame()))
  expect_true(is_empty(list(NULL)))

  # NA vector
  x <- rep(NA, 5)
  expect_true(all(is_empty(x)))
  expect_false(all(is_empty(x, na_empty = FALSE)))
})


test_that("text has nasty elements", {
  x <- "Some patchy proctitis but also inflamed terminal ileum compatible with crohn\x92s."
  rm_invalid_utf8_(x)
  iconv(x, "UTF-8", "UTF-8", sub = "*")
  expect_false(is_empty("Some patchy proctitis but also inflamed terminal ileum compatible with crohn\x92s."))
})
