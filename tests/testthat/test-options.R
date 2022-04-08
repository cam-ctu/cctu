

test_that("Check defaults", {
  o <- cctu_options()
  expect_equal(names(o), c('sumby_count', 'print_plot', 'number',
                           'nested_run_batch', 'rounding_fn', 'digits_pct',
                           'missing_report_data', 'dlu', 'digits',
                           'subjid_string'))

  expect_equal(cctu_options("digits_pct"), 0)
  expect_null(cctu_options("dlu"))
  expect_equal(cctu_options("subjid_string"), "subjid")

  expect_equal(cctu_options("sumby_count"), 0)

})

test_that("Check assign", {

  cctu_options(testname = 22)
  expect_equal(cctu_options("testname"), 22)

  cctu_options(digits = 10)
  expect_equal(cctu_options("digits"), 10)

  cctu_options(digits_pct = 3)
  expect_equal(cctu_options("digits_pct"), 3)

  cctu_options(sumby_count = 10)
  expect_equal(cctu_options("sumby_count"), 0)

  cctu_options(a = 3, b = 4)
  expect_invisible(cctu_options("a", "b"))
  expect_equal(cctu_options("a", "b"), list(a = 3, b = 4))

  expect_warning(cctu_options(a = 3, b = 4, "c"),
                 "cctu option 'c' not found")

  expect_error(cctu_options(c),
               "cctu_options only accepts arguments as 'name=value' or 'name'")

  init_cctu_env()

  o <- cctu_options()
  expect_equal(cctu_options("digits"), 3)
  expect_equal(cctu_options("digits_pct"), 0)
  expect_warning(cctu_options("testname"),
                 "cctu option 'testname' not found")
  expect_warning(cctu_options("a"),
                 "cctu option 'a' not found")

})





