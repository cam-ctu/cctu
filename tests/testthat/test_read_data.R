context("read_data")

data_table <- data.frame(
  name = c("dirtydata", "meta"),
  file = c(
    "dirtydata.csv",
    "meta_table.xlsx"
  ),
  folder = system.file("extdata", package = "cctu"),
  stringsAsFactors = FALSE
)




test_that("basic case", {
  expect_is(data_table_summary(data_table), "data.frame")
  options("verbose" = TRUE)

  expect_false(exists("dirtydata"))
  expect_false(exists("meta"))
  read_data(data_table)
  expect_true(exists("dirtydata"))
  expect_true(exists("meta"))
  rm(dirtydata, meta)
  data2 <- data_table
  names(data2) <- c("link", "path", "folder")
  read_data(data2, name_variable = "link", file_variable = "path")
  expect_true(exists("dirtydata"))
  expect_true(exists("meta"))
  rm(dirtydata, meta)
})

test_that("non standard case", {
  X <- read_data(data_table[1, ], frame = NULL, stringsAsFactors = FALSE)
  expect_false(is.factor(X$gender))
  expect_error(read_data("dirtydata", data_table, fun = readxl::read_excel, frame = NULL))
  expect_warning(
    read_data("dirtydata", data_table, fun = print, remove_blank_rows_cols_option = FALSE, frame = NULL),
    "this function is designed to be used for reading in data. You are calling: print"
  )


  old_fn <- base::requireNamespace
  myrequireNamespace <- function(...) FALSE
  unlockBinding("requireNamespace", as.environment("package:base"))
  assign("requireNamespace", myrequireNamespace, "package:base")
  expect_error(read_data("meta", data_table, frame = NULL), "Package \"readxl\" needed for this function to load excel files")
  assign("requireNamespace", old_fn, "package:base")
  lockBinding("requireNamespace", as.environment("package:base"))
  rm(old_fn, myrequireNamespace)
})


test_that("interaction with apply_macro_dict", {
  my_data <- data.frame(
    name = c("dt", "dlu", "clu"),
    file = c("pilotdata.csv", "pilotdata_dlu.csv", "pilotdata_clu.csv"),
    folder = system.file("extdata", package = "cctu")
  )

  read_data(my_data, colClasses = "character")
  dt$subjid <- substr(dt$USUBJID, 8, 11)
  # Apply CLU and DLU files

  expect_s3_class(apply_macro_dict(dt, dlu = dlu, clu = clu), "data.table")
  dt <- apply_macro_dict(dt, dlu = dlu, clu = clu)
  expect_true("arm" %in% names(dt))
  expect_false("arm" %in% dlu$shortcode)
  # this assume the dlu & clu have teh column names cleaned, as well
  # as cleaning the content of "shortcode". shortcode -> shortcode
  # expect_flase("arm" %in% clu$shortcode)
  clean_dlu <- tidy_dlu(dlu)
  expect_true("arm" %in% clean_dlu$shortcode)

  expect_type(dt$arm, "double")
  expect_true(has_labels(dt$arm))
  # there's no columns that are dates, to check their conversion,
  # but I'm not so bothered about these.
  read_data(my_data, colClasses = "character", clean_names = TRUE)
  # The remove_blank_rows_cols, woudl be applicable when a
  # trial is early in its life. No patients have doen the later visits
  # So MACRO does not export them , and or they might be deleted
  # does apply_macro_dict look to see what is in the data, and then
  # look up the clu/dlu to convert, and do it in that order?
  dt$subjid <- substr(dt$usubjid, 8, 11)
  # expect_error( apply_macro_dict(dt, dlu = dlu, clu = clu))
})

test_that("relative file paths", {
  df <- data.frame(
    name = c("check"),
    file = c("../testthat/test_read_data.R"),
    folder = test_path()
  )
  df_summary <- data_table_summary(df)
  expect_false(grepl("\\.\\.", df_summary$full_file_path[1]))
})
