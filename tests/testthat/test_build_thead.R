test_that("spanner_sep = NULL returns the single-row header unchanged", {
  expect_equal(
    build_thead(c("Variable", "A", "B"), spanner_sep = NULL),
    "<thead>\n<tr><th>Variable</th><th>A</th><th>B</th></tr>\n</thead>\n"
  )
})

test_that("spanner builds a two-row grouped header", {
  out <- build_thead(
    c("Variable", "Placebo_Normal", "Placebo_Abnormal",
      "Research_Normal", "Research_Abnormal"),
    spanner_sep = "_"
  )
  expect_equal(
    out,
    paste0(
      "<thead>\n",
      "<tr><th vmerge='restart'>Variable</th>",
      "<th colspan='2' rborder='1'>Placebo</th><th colspan='2'>Research</th></tr>\n",
      "<tr><th vmerge='cont'></th>",
      "<th>Normal</th><th rborder='1'>Abnormal</th>",
      "<th>Normal</th><th>Abnormal</th></tr>\n",
      "</thead>\n"
    )
  )
})

test_that("ungrouped data columns vMerge across both header rows", {
  out <- build_thead(c("Var", "G_a", "G_b", "plain"), spanner_sep = "_")
  expect_equal(
    out,
    paste0(
      "<thead>\n",
      "<tr><th vmerge='restart'>Var</th>",
      "<th colspan='2'>G</th><th vmerge='restart'>plain</th></tr>\n",
      "<tr><th vmerge='cont'></th>",
      "<th>a</th><th>b</th><th vmerge='cont'></th></tr>\n",
      "</thead>\n"
    )
  )
})

test_that("split happens on the first separator only; sep inside leaf is kept", {
  out <- build_thead(c("Var", "Research_No_post-baseline"), spanner_sep = "_")
  expect_equal(
    out,
    paste0(
      "<thead>\n",
      "<tr><th vmerge='restart'>Var</th>",
      "<th colspan='1'>Research</th></tr>\n",
      "<tr><th vmerge='cont'></th>",
      "<th>No_post-baseline</th></tr>\n",
      "</thead>\n"
    )
  )
})

test_that("non-adjacent equal groups stay separate", {
  out <- build_thead(c("V", "A_1", "B_1", "A_2"), spanner_sep = "_")
  expect_equal(
    out,
    paste0(
      "<thead>\n",
      "<tr><th vmerge='restart'>V</th>",
      "<th colspan='1' rborder='1'>A</th>",
      "<th colspan='1' rborder='1'>B</th><th colspan='1'>A</th></tr>\n",
      "<tr><th vmerge='cont'></th>",
      "<th rborder='1'>1</th><th rborder='1'>1</th><th>2</th></tr>\n",
      "</thead>\n"
    )
  )
})

test_that("a single (stub-only) column ignores spanner and stays single-row", {
  expect_equal(
    build_thead("Variable", spanner_sep = "_"),
    "<thead>\n<tr><th>Variable</th></tr>\n</thead>\n"
  )
})

test_that("write_table(spanner) emits a two-row header on the styled path", {
  .old_meta <- set_meta_table(cctu::meta_table_example)
  on.exit(set_meta_table(.old_meta), add = TRUE)
  cctu_env$parent <- "test"

  x <- data.frame(
    label            = c("Normal", "Abnormal"),
    Placebo_Normal   = c("3", "1"),
    Placebo_Abnormal = c("0", "2"),
    Research_Normal  = c("4", "0"),
    Research_Abnormal = c("1", "3"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  x <- format_table(x)

  dir <- tempdir()
  write_table(x, number = "1.1", directory = dir, spanner_sep = "_",
              clean_up = FALSE)
  tab <- xml2::read_xml(file.path(dir, "table_1.1.xml"))

  head_rows <- xml2::xml_find_all(tab, "//thead/tr")
  expect_length(head_rows, 2)

  # Top row: vMerge-restart corner + two arm spanners with colspan 2.
  top_th <- xml2::xml_find_all(head_rows[[1]], "th")
  expect_equal(xml2::xml_attr(top_th[[1]], "vmerge"), "restart")
  expect_equal(xml2::xml_text(top_th[[2]]), "Placebo")
  expect_equal(xml2::xml_attr(top_th[[2]], "colspan"), "2")
  # Separator between the two adjacent arm groups.
  expect_equal(xml2::xml_attr(top_th[[2]], "rborder"), "1")
  expect_equal(xml2::xml_text(top_th[[3]]), "Research")

  # Bottom row: vMerge-continue corner + four leaf labels.
  bot_th <- xml2::xml_find_all(head_rows[[2]], "th")
  expect_equal(xml2::xml_attr(bot_th[[1]], "vmerge"), "cont")
  expect_equal(xml2::xml_text(bot_th[[2]]), "Normal")
})

test_that("write_table without spanner is unchanged on the plain path", {
  .old_meta <- set_meta_table(cctu::meta_table_example)
  on.exit(set_meta_table(.old_meta), add = TRUE)
  cctu_env$parent <- "test"

  x <- data.frame(a = 1, b = 2)
  d1 <- tempfile("nospan"); dir.create(d1)
  d2 <- tempfile("span"); dir.create(d2)
  write_table(x, number = "1.1", directory = d1, clean_up = FALSE)
  write_table(x, number = "1.1", directory = d2, spanner_sep = NULL,
              clean_up = FALSE)
  expect_identical(
    readLines(file.path(d1, "table_1.1.xml")),
    readLines(file.path(d2, "table_1.1.xml"))
  )
  # Header is still a single row.
  tab <- xml2::read_xml(file.path(d1, "table_1.1.xml"))
  expect_length(xml2::xml_find_all(tab, "//thead/tr"), 1)
})

test_that("a spanned table round-trips into a valid .docx", {
  .old_meta <- set_meta_table(cctu::meta_table_example)
  on.exit(set_meta_table(.old_meta), add = TRUE)
  cctu_env$parent <- "test"

  x <- data.frame(
    label             = c("Normal", "Abnormal"),
    Placebo_Normal    = c("3", "1"),
    Placebo_Abnormal  = c("0", "2"),
    Research_Normal   = c("4", "0"),
    Research_Abnormal = c("1", "3"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  x <- format_table(x)
  tdir <- tempfile("core"); dir.create(tdir)
  write_table(x, number = "1.1", directory = tdir, spanner_sep = "_",
              clean_up = FALSE)

  filetemp <- tempfile("report", fileext = ".docx")
  on.exit(unlink(filetemp), add = TRUE)
  write_docx("test report", "author",
    meta_table = get_meta_table() %>% dplyr::filter(number == "1.1"),
    popn_labels = "my population",
    filename = filetemp,
    table_path = tdir
  )
  expect_true(file.exists(filetemp))
})
