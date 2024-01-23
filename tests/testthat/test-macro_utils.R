# Test MACRO util functions

dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"),
               colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

# dt <- read.csv("inst/extdata/pilotdata.csv")
# dlu <- read.csv("inst/extdata/pilotdata_dlu.csv")
# clu <- read.csv("inst/extdata/pilotdata_clu.csv")

dt$subjid <- substr(dt$USUBJID, 8, 11)

test_that("Apply DLU and CLU files", {
  df <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)
  expect_s3_class(df, "data.table")
  expect_identical(var_lab(df$ARM), "Treatment Arm")
  expect_identical(val_lab(df$ARM), c("Placebo" = 1L, "Research" = 2L))

  expect_equal(nrow(cctu_env$dlu), nrow(dlu))
  expect_identical(names(cctu_env$dlu), tolower(c("ShortCode", "Description", "Type",
                                                  "Visit", "Form", "Question")))

  expect_true(is.numeric(df$BMIBL))

  # Lower case
  df <- apply_macro_dict(dt, dlu, clu, clean_names = TRUE)
  expect_s3_class(df, "data.table")
  expect_identical(var_lab(df$arm), "Treatment Arm")
  expect_identical(val_lab(df$arm), c("Placebo" = 1L, "Research" = 2L))

  # CLU not category
  dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
  clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))
  clu$CatValue[clu$ShortCode == "ARM"][1] <- ""
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE),
               "Variable ARM has empty category value")

  dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
  clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))
  clu$CatValue[clu$ShortCode == "ARM"][1] <- NA
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu),
               "Variable arm has empty category value")

  dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
  clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))
  clu <- clu[, -1]
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu),
               "Variable shortcode not found in the clu data")

  dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
  clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))
  colnames(dlu) <- paste("var", 1:ncol(dlu))
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu),
               "Variable shortcode, description, type not found in the dlu data")

  dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
  clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))
  dlu[[2]] <- dlu[[1]]
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu),
               "The second variable of the DLU file must be in the original")


})


test_that("Extract form", {
  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)
  lb <- extract_form(df, "Lab")
  expect_equal(names(lb)[1:4], toupper(c("avisit", "bili", "alt", "perf")))
  expect_true(all(unique(lb$form_visit) %in% c("SCREENING", "TRT", "ENDTRT")))

  expect_true(all(levels(to_factor(lb$AVISIT)) %in% c('Baseline','Week 4','Week 8','Week 16','End of Treatment')))

  lb <- extract_form(df, "Lab", visit = "SCREENING")
  expect_identical(unique(lb$form_visit), "SCREENING")

  expect_error(extract_form(df, "SCREENING"),
               "Form name SCREENING can not be found in the DLU file")

  expect_error(extract_form(df, c("PatientReg", "Lab")),
               "Form must be of length 1")

  lb <- extract_form(df, "Lab", visit = c("SCREENING", "TRT"))
  expect_true(all(unique(lb$form_visit) %in% c("SCREENING", "TRT")))

  expect_error(extract_form(df, "Lab", visit = c("SCREENING", "TEST")),
               "Visit name TEST can not be found in the DLU file")

  df <- apply_macro_dict(dt, dlu, clu)
  lb <- extract_form(df, "Lab")
  expect_equal(names(lb)[1:4], c("avisit", "bili", "alt", "perf"))

})



test_that("Remove empty cols and rows", {

  dt$AGE <- NA
  dt[20, ] <- NA

  # Remove empty columns and rows
  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)
  expect_equal(dim(df), dim(dt)-1)

  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE, rm_empty = "rows")
  expect_equal(nrow(df), nrow(dt)-1)
  expect_equal(ncol(df), ncol(dt))

  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE, rm_empty = "cols")
  expect_equal(ncol(df), ncol(dt)-1)
  expect_equal(nrow(df), nrow(dt))

  options(cctu_rm_empty = "none")
  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)
  expect_equal(dim(df), dim(dt))

  # Remove empty columns and rows for extract_form
  options(cctu_rm_empty = "both")
  ptreg <- extract_form(df, "PatientReg")
  expect_equal(dim(ptreg), c(sum(!is_empty(df$ARM)), 5))

  ptreg <- extract_form(df, "PatientReg", rm_empty = "rows")
  expect_equal(dim(ptreg), c(sum(!is_empty(df$ARM)), 6))


})


test_that("Test date conversion", {

  dat <- data.frame(
    mdy = c("2015/06/28", "2016/11/25", NA),
    mdy.time = c("2015/06/28 06:13:10", "2016/11/25 18:13:10", NA),
    dmy.part = c("06/2015", "25/11/2016", NA),
    dmy = c("28/06/2015", "", "25/11/2016"),
    time = c("06:13:10", "18:13:10", NA)
  )

  dlu <- data.frame(
    shortcode = c("mdy", "mdy.time", "dmy.part", "dmy", "time"),
    description = c("mdy", "mdy.time", "dmy.part", "dmy", "time"),
    type = rep("Date", 5),
    visit = rep("COVER", 5),
    form = rep("PatientReg", 5),
    question = c("mdy", "mdy.time", "dmy.part", "dmy", "time")
  )

  df <- apply_macro_dict(dat, dlu)

  # Should not be converted
  expect_type(df$dmy_part, "character")
  expect_equivalent(df$dmy_part, dat$dmy.part)

  expect_is(df$mdy_time, "Date")
  expect_is(df$dmy, "Date")


  # Test with hour
  df <- apply_macro_dict(dat, dlu,
                         date_format = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d %H:%M:%S"))

  expect_is(df$mdy_time, "POSIXct")
  expect_is(df$mdy, "character")
  expect_equivalent(df$mdy, dat$mdy)
  expect_is(df$dmy, "Date")


})



