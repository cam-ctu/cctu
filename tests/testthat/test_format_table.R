test_that("format_table assembles row_style tokens in the documented order", {
  df <- data.frame(label = c("Group A", "n", "mean"),
                   value = c("", "10", "3.4"),
                   stringsAsFactors = FALSE)

  out <- format_table(df, bold = 1, bgcol = 1, span = 1, indent = 2:3)

  expect_identical(
    attr(out, "row_style"),
    c("bold;bgcol;span", "indent", "indent")
  )
  # The data shape is preserved.
  expect_identical(out[, c("label", "value")], df)
})

test_that("format_table accepts named and hex colours for bgcol/col", {
  df <- data.frame(label = c("a", "b"), value = c("1", "2"),
                   stringsAsFactors = FALSE)

  # Named colour via grDevices::col2rgb.
  out1 <- format_table(df, bgcol = 1, bgcol_color = "steelblue")
  expect_match(attr(out1, "row_style")[1], "bgcol:4682B4", fixed = TRUE)

  # Hex with leading "#" gets normalised to upper-case, no "#".
  out2 <- format_table(df, bgcol = 1, bgcol_color = "#ff0000")
  expect_match(attr(out2, "row_style")[1], "bgcol:FF0000", fixed = TRUE)

  # Bare 6-char hex also accepted.
  out3 <- format_table(df, bgcol = 1, bgcol_color = "00ff00")
  expect_match(attr(out3, "row_style")[1], "bgcol:00FF00", fixed = TRUE)

  # col_color follows the same rules.
  out4 <- format_table(df, col = 1:2, col_color = "red")
  expect_match(attr(out4, "row_style")[1], "col:FF0000", fixed = TRUE)
  expect_match(attr(out4, "row_style")[2], "col:FF0000", fixed = TRUE)
})

test_that("format_table warns and drops `col` when col_color is NULL", {
  df <- data.frame(label = c("a", "b"), value = c("1", "2"),
                   stringsAsFactors = FALSE)
  expect_warning(
    out <- format_table(df, col = 1:2),
    "col_color"
  )
  # No `col` token leaks into the output.
  expect_false(any(grepl("col:", attr(out, "row_style"))))
})

test_that("format_table validates row indices", {
  df <- data.frame(label = c("a", "b"), value = c("1", "2"),
                   stringsAsFactors = FALSE)

  expect_error(format_table(df, bold = 0L),    "outside")
  expect_error(format_table(df, bold = 3L),    "outside")
  expect_error(format_table(df, bold = NA_integer_), "NA")
  expect_error(format_table(df, bold = "a"),   "numeric")
})

test_that("format_table rejects non-scalar / non-character colours", {
  df <- data.frame(label = "a", value = "1", stringsAsFactors = FALSE)
  expect_error(
    format_table(df, bgcol = 1, bgcol_color = c("red", "blue")),
    "single character string"
  )
  expect_error(
    format_table(df, bgcol = 1, bgcol_color = "not-a-real-colour"),
    "not a valid color"
  )
})

test_that("format_table produces empty row_style strings for un-touched rows", {
  df <- data.frame(label = c("a", "b", "c"), value = c("1", "2", "3"),
                   stringsAsFactors = FALSE)
  out <- format_table(df, bold = 2)
  expect_identical(attr(out, "row_style"), c("", "bold", ""))
})

test_that("format_table works on matrix input", {
  m <- matrix(c("1", "2", "3", "4"), nrow = 2,
              dimnames = list(c("a", "b"), c("x", "y")))
  out <- format_table(m, bold = 1, bgcol = 1)
  expect_identical(attr(out, "row_style"), c("bold;bgcol", ""))
  expect_true(is.matrix(out))
})
