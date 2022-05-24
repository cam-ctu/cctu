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
  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)
  expect_s3_class(df, "data.table")
  expect_identical(var_lab(df$ARM), "Treatment Arm")
  expect_identical(val_lab(df$ARM), c("Placebo" = 1L, "Research" = 2L))

  expect_equal(nrow(cctu_env$dlu), nrow(dlu))
  expect_identical(names(cctu_env$dlu), c("ShortCode", "Description", "Type",
                                          "Visit", "Form", "Question"))

  expect_true(is.numeric(df$BMIBL))

  # Lower case
  df <- apply_macro_dict(dt, dlu, clu, clean_names = TRUE)
  expect_s3_class(df, "data.table")
  expect_identical(var_lab(df$arm), "Treatment Arm")
  expect_identical(val_lab(df$arm), c("Placebo" = 1L, "Research" = 2L))

  # CLU not category
  tmp <- clu
  tmp$CatValue[clu$ShortCode == "ARM"][1] <- ""
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = tmp, clean_names = FALSE),
               "Variable ARM has empty category values")

  tmp$CatValue[clu$ShortCode == "ARM"][1] <- NA
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = tmp),
               "Variable arm has empty category values")

  colnames(clu)[1] <- tolower(colnames(clu)[1])
  expect_error(apply_macro_dict(dt, dlu = dlu, clu = clu),
               "Variable ShortCode not found in the clu data")

  tmp_dlu <- dlu
  colnames(tmp_dlu) <- tolower(colnames(tmp_dlu))
  expect_error(apply_macro_dict(dt, dlu = tmp_dlu, clu = clu),
               "Variable ShortCode, Description, Type not found in the dlu data")

})


test_that("Extract form", {
  df <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)
  lb <- extract_form(df, "Lab")
  expect_equal(names(lb)[1:4], toupper(c("avisit", "bili", "alt", "perf")))
  expect_true(all(unique(lb$FormVisit) %in% c("SCREENING", "TRT", "ENDTRT")))

  expect_true(all(levels(to_factor(lb$AVISIT)) %in% c('Baseline','Week 4','Week 8','Week 16','End of Treatment')))

  lb <- extract_form(df, "Lab", visit = "SCREENING")
  expect_identical(unique(lb$FormVisit), "SCREENING")

  expect_error(extract_form(df, "SCREENING"),
               "Form name SCREENING can not be found in the DLU file")

  expect_error(extract_form(df, c("PatientReg", "Lab")),
               "Form must be of length 1")

  lb <- extract_form(df, "Lab", visit = c("SCREENING", "TRT"))
  expect_true(all(unique(lb$FormVisit) %in% c("SCREENING", "TRT")))

  expect_error(extract_form(df, "Lab", visit = c("SCREENING", "TEST")),
               "Visit name TEST can not be found in the DLU file")

  df <- apply_macro_dict(dt, dlu, clu)
  lb <- extract_form(df, "Lab")
  expect_equal(names(lb)[1:4], c("avisit", "bili", "alt", "perf"))

})

