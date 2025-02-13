context("Test remove_blank_rows_cols")


dirty <- read.csv(system.file("extdata", "dirtydata.csv", package = "cctu"), stringsAsFactors = FALSE)


test_that("default use", {
  expect_message(dirty2 <- remove_blank_rows_cols(dirty, verbose = TRUE), "dirty modified with blanks removed.")
  expect_equal(dim(dirty), c(6, 6))
  # to check the invisible assignment
  expect_equal(dim(dirty2), c(6, 6))
})

test_that(
  "rows and cols alone",
  {
    dirty <- read.csv(system.file("extdata", "dirtydata.csv", package = "cctu"), stringsAsFactors = FALSE)
    cols_only <- remove_blank_rows_cols(dirty, which = "cols", convert = FALSE)
    expect_equal(dim(dirty), c(8, 8))
    expect_equal(dim(cols_only), c(8, 6))
    remove_blank_rows_cols(dirty, which = "rows", convert = TRUE)
    expect_equal(dim(dirty), c(6, 8))
    # expect_warning( remove_blank_rows_cols(dirty, cols=FALSE, rows=FALSE))
    # expect_equal(dim(dirty), c(6,8))
  }
)
