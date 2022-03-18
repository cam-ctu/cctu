# Need to add more

dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"))
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

dt$subjid <- substr(dt$USUBJID, 8, 11)
dt <- apply_lus(dt, dlu, clu)

set_meta_table(cctu::meta_table_example)

#Create the population table
popn <- dt[,"subjid", drop=FALSE]
popn$safety <- TRUE

create_popn_envir("dt", popn)

tmp_dir <- tempdir()
#tidy up
rm(dlu)
.reserved <- ls()


test_that("Start from data reading", {

  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  expect_true("subjid" %in% names(df))

  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(vars = c("AGE", "SEX", "BMIBL"),
             group = "ARM",
             data = df,
             select = c("BMIBL" = "RACEN != 1"))

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$subject_ID, "1275")
  reset_missing_report()
  expect_equal(nrow(get_missing_report()), 0)

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(test_path("ref", "table_ctab1.xml"),
                                file.path(tmp_dir, "table_1.1.xml")))

})

test_that("Variable groups", {

  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  base_lab <- extract_form(dt, "Lab", visit = "SCREENING",
                       vars_keep = c("subjid"))

  base_lab$ABNORMALT <- base_lab$ALT > 22.5
  var_lab(base_lab$ABNORMALT) <- "ALT abnormal"
  base_lab$ABNORMAST <- base_lab$AST > 25.5
  var_lab(base_lab$ABNORMAST) <- "AST abnormal"

  df <- merge(df, base_lab, by = "subjid")

  expect_identical(var_lab(df$ABNORMAST), "AST abnormal")

  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(vars = list(c("AGE", "SEX", "BMIBL"),
                         "Blood" = c("ALT", "AST"),
                         "Patients with Abnormal" = c("ABNORMAST", "ABNORMALT")),
             group = "ARM",
             data = df,
             select = c("BMIBL" = "RACEN != 1",
                        "ALT" = "PERF == 1"))

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$form[4:5], rep("Derived", 2))

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(test_path("ref", "table_ctab2.xml"),
                                file.path(tmp_dir, "table_1.1.xml")))

})



test_that("By cycle summary", {

  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  expect_true("ARM" %in% names(df))

  X <- cttab(vars = c("AST", "BILI", "ALT"),
             group = "ARM",
             data = df,
             row_split = "AVISIT",
             select = c("ALT" = "PERF == 1"))

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$subject_ID[1], "1275")
  expect_identical(mis_rp$subject_ID[7], "1181, 1286, 1259")

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(test_path("ref", "table_ctab3.xml"),
                                file.path(tmp_dir, "table_1.10.xml")))

})

test_that("No treatment arm", {

  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(vars = c("AGE", "SEX", "BMIBL"),
             data = df,
             select = c("BMIBL" = "RACEN != 1"))

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(test_path("ref", "table_ctab4.xml"),
                                file.path(tmp_dir, "table_1.1.xml")))

})


test_that("Check errors", {

  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  # Duplicated variables
  expect_error(cttab(vars = c("AST", "BILI", "ALT", "ALT"),
                     group = "ARM",
                     data = df,
                     row_split = "AVISIT",
                     select = c("ALT" = "PERF == 1")),
               "vars, group and row_splot duplicated.")

  expect_error(cttab(vars = c("AST", "BILI", "ALT", "ARM"),
                     group = "ARM",
                     data = df,
                     row_split = "AVISIT",
                     select = c("ALT" = "PERF == 1")),
               "vars, group and row_splot duplicated.")

  # Extra variables not in the dataset
  expect_error(cttab(vars = c("AST", "BILI", "ALT", "MPG"),
                     group = "ARM",
                     data = df,
                     row_split = "AVISIT",
                     select = c("ALT" = "PERF == 1")),
               "Variable MPG not in the dataset, please check!")

  expect_error(stat_tab(vars = c("AST", "BILI", "ALT", "MPG"),
                     group = "ARM",
                     data = df,
                     select = c("ALT" = "PERF == 1")),
               "Variable MPG not in the dataset, please check!")

})

