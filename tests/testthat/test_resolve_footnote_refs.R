context("resolve_footnote_refs")

test_that("@ref{} resolves a table reference to an xref element", {
  mt <- data.frame(
    number = c("1.1", "2.3"),
    item   = c("table", "figure"),
    stringsAsFactors = FALSE
  )
  result <- resolve_footnote_refs("See @ref{1.1} for details.", mt)
  expect_equal(
    result,
    "See <xref bookmark=\"Table_11\" label=\"Table 1.1\"/> for details."
  )
})

test_that("@ref{} resolves a figure reference to an xref element", {
  mt <- data.frame(
    number = c("1.1", "2.3"),
    item   = c("table", "figure"),
    stringsAsFactors = FALSE
  )
  result <- resolve_footnote_refs("Shown in @ref{2.3}.", mt)
  expect_equal(
    result,
    "Shown in <xref bookmark=\"Figure_23\" label=\"Figure 2.3\"/>."
  )
})

test_that("multiple @ref{} tokens in one string are all resolved", {
  mt <- data.frame(
    number = c("1.1", "2.3"),
    item   = c("table", "figure"),
    stringsAsFactors = FALSE
  )
  result <- resolve_footnote_refs("See @ref{1.1} and @ref{2.3}.", mt)
  expect_equal(
    result,
    paste0(
      "See <xref bookmark=\"Table_11\" label=\"Table 1.1\"/> and ",
      "<xref bookmark=\"Figure_23\" label=\"Figure 2.3\"/>."
    )
  )
})

test_that("item type is matched case-insensitively", {
  mt <- data.frame(
    number = "1.1",
    item   = "Figure",
    stringsAsFactors = FALSE
  )
  result <- resolve_footnote_refs("See @ref{1.1}.", mt)
  expect_equal(
    result,
    "See <xref bookmark=\"Figure_11\" label=\"Figure 1.1\"/>."
  )
})

test_that("dotted numbers strip all dots for the bookmark name", {
  mt <- data.frame(
    number = "2.2.3",
    item   = "table",
    stringsAsFactors = FALSE
  )
  result <- resolve_footnote_refs("See @ref{2.2.3}.", mt)
  expect_equal(
    result,
    "See <xref bookmark=\"Table_223\" label=\"Table 2.2.3\"/>."
  )
})

test_that("unknown @ref{} warns and substitutes a placeholder", {
  mt <- data.frame(
    number = "1.1",
    item   = "table",
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- resolve_footnote_refs("See @ref{9.9}.", mt),
    "@ref\\{9.9\\} not found in meta_table"
  )
  expect_equal(result, "See [ref: 9.9].")
})

test_that("footnote without @ref{} is returned unchanged", {
  mt <- data.frame(
    number = "1.1",
    item   = "table",
    stringsAsFactors = FALSE
  )
  fn <- "No reference here."
  expect_equal(resolve_footnote_refs(fn, mt), fn)
})

test_that("resolve_footnote_refs is vectorised over footnotes", {
  mt <- data.frame(
    number = c("1.1", "2.3"),
    item   = c("table", "figure"),
    stringsAsFactors = FALSE
  )
  fns <- c("See @ref{1.1}.", "See @ref{2.3}.", "No ref.")
  result <- resolve_footnote_refs(fns, mt)
  expect_equal(
    result,
    c(
      "See <xref bookmark=\"Table_11\" label=\"Table 1.1\"/>.",
      "See <xref bookmark=\"Figure_23\" label=\"Figure 2.3\"/>.",
      "No ref."
    )
  )
})
