# Need to add more

dt <- read.csv(system.file("extdata", "pilotdata.csv", package = "cctu"))
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package = "cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package = "cctu"))

dt$subjid <- substr(dt$USUBJID, 8, 11)
dt <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)

set_meta_table(cctu::meta_table_example)

# Create the population table
popn <- dt[, "subjid", drop = FALSE]
popn$safety <- TRUE

create_popn_envir("dt", popn)

tmp_dir <- tempdir()
# tidy up
rm(dlu)
.reserved <- ls()

options("cctu_digits_pct" = 1)

test_that("Start from data reading", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  expect_true("subjid" %in% names(df))

  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(
    x = c("AGE", "SEX", "BMIBL"),
    group = "ARM",
    data = df,
    select = c("BMIBL" = "RACEN != 1")
  )

  X1 <- cttab(AGE + SEX + BMIBL ~ ARM,
    data = df,
    select = c("BMIBL" = "RACEN != 1")
  )

  expect_identical(X, X1)

  testthat_print(X)

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$subject_id, "1275")
  reset_missing_report()
  expect_equal(nrow(get_missing_report()), 0)

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab1.xml"),
    file.path(tmp_dir, "table_1.1.xml")
  ))
})

test_that("Variable groups", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  base_lab <- extract_form(dt, "Lab",
    visit = "SCREENING",
    vars_keep = c("subjid")
  )

  base_lab$ABNORMALT <- base_lab$ALT > 22.5
  var_lab(base_lab$ABNORMALT) <- "ALT abnormal"
  base_lab$ABNORMAST <- base_lab$AST > 25.5
  var_lab(base_lab$ABNORMAST) <- "AST abnormal"

  df <- merge(df, base_lab, by = "subjid")

  expect_identical(var_lab(df$ABNORMAST), "AST abnormal")

  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(
    x = list(c("AGE", "SEX", "BMIBL"),
      "Blood" = c("ALT", "AST"),
      "Patients with Abnormal" = c("ABNORMAST", "ABNORMALT")
    ),
    group = "ARM",
    data = df,
    select = c(
      "BMIBL" = "RACEN != 1",
      "ALT" = "PERF == 1"
    )
  )

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$form[4:5], rep("Derived", 2))

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir, clean_up = FALSE)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab2.xml"),
    file.path(tmp_dir, "table_1.1.xml")
  ))

  dmp_path <- tempfile(fileext = ".csv")
  dump_missing_report(dmp_path)
  expect_true(file.exists(dmp_path))

  # No imputation
  X1 <- cttab(
    x = c("ABNORMAST", "ABNORMALT"),
    data = df,
    logical_na_impute = NA
  )
  expect_equal(unname(X1[, 1]), c("30/99 (30.3%)", "20/85 (23.5%)"))

  # Impute with TRUE
  X <- cttab(
    x = c("ABNORMAST", "ABNORMALT"),
    data = df,
    logical_na_impute = TRUE
  )

  df$ABNORMALT[is.na(df$ABNORMALT)] <- TRUE
  df$ABNORMAST[is.na(df$ABNORMAST)] <- TRUE
  X2 <- cttab(
    x = c("ABNORMAST", "ABNORMALT"),
    data = df
  )
  expect_identical(X2, X)
})



test_that("By cycle summary", {
  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  expect_true("ARM" %in% names(df))
  df$inrange <- ifelse(df$AST < 20, "Low",
    ifelse(df$AST > 40, "High", "Normal")
  )
  var_lab(df$inrange) <- "AST range"

  X <- cttab(
    x = c("AST", "BILI", "ALT", "inrange"),
    group = "ARM",
    data = df,
    row_split = "AVISIT",
    select = c("ALT" = "PERF == 1")
  )

  X1 <- cttab(AST + BILI + ALT + inrange ~ ARM | AVISIT,
    data = df,
    select = c("ALT" = "PERF == 1")
  )

  expect_identical(X, X1)

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$visit[6], "Baseline")
  expect_identical(mis_rp$subject_id[4], "1160")
  expect_identical(mis_rp$form[4], "Derived")

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab3.xml"),
    file.path(tmp_dir, "table_1.10.xml")
  ))
})

test_that("By cycle No treatment arm summary", {
  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  df$ABNORMALT <- df$ALT > 22.5
  var_lab(df$ABNORMALT) <- "ALT abnormal"
  df$ABNORMAST <- df$AST > 25.5
  var_lab(df$ABNORMAST) <- "AST abnormal"

  X <- cttab(
    x = c("AST", "BILI", "ALT", "ABNORMALT", "ABNORMAST"),
    data = df,
    row_split = "AVISIT",
    select = c("ALT" = "PERF == 1")
  )

  X1 <- cttab(AST + BILI + ALT + ABNORMALT + ABNORMAST ~ 1 | AVISIT,
    data = df,
    select = c("ALT" = "PERF == 1")
  )

  expect_identical(X, X1)


  X <- cttab(
    x = list(c("AST", "BILI", "ALT"),
      "Abnormal" = c("ABNORMALT", "ABNORMAST")
    ),
    data = df,
    row_split = "AVISIT",
    select = c("ALT" = "PERF == 1")
  )

  mis_rp <- get_missing_report()
  expect_identical(mis_rp$subject_id[1], "1275")
  expect_identical(mis_rp$subject_id[7], "1181, 1286, 1259")

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab5.xml"),
    file.path(tmp_dir, "table_1.10.xml")
  ))

  # Report blinded should be same as no grouping
  X1 <- cttab(
    x = list(c("AST", "BILI", "ALT"),
      "Abnormal" = c("ABNORMALT", "ABNORMAST")
    ),
    data = df,
    group = "ARM",
    row_split = "AVISIT",
    select = c("ALT" = "PERF == 1"),
    blinded = TRUE
  )
  expect_identical(X1, X)
})


test_that("No treatment arm and cycle", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  df$over_w <- df$BMIBL > 28
  df$hi_age <- df$AGE > 70


  df$BMIBL[df$RACEN == 6] <- NA

  X <- cttab(
    x = c("AGE", "SEX", "BMIBL", "over_w", "hi_age"),
    data = df,
    select = c("BMIBL" = "RACEN != 1")
  )

  X1 <- cttab(AGE + SEX + BMIBL + over_w + hi_age ~ 1,
    data = df,
    select = c("BMIBL" = "RACEN != 1")
  )

  expect_identical(X, X1)

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab4.xml"),
    file.path(tmp_dir, "table_1.1.xml")
  ))
})


test_that("Check errors", {
  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  # Duplicated variables
  expect_error(
    cttab(
      x = c("AST", "BILI", "ALT", "ALT"),
      group = "ARM",
      data = df,
      row_split = "AVISIT",
      select = c("ALT" = "PERF == 1")
    ),
    "The variable list, group or row split variable have duplicated variable."
  )

  expect_error(
    cttab(
      x = c("AST", "BILI", "ALT", "ARM"),
      group = "ARM",
      data = df,
      row_split = "AVISIT",
      select = c("ALT" = "PERF == 1")
    ),
    "The variable list, group or row split variable have duplicated variable."
  )

  # Extra variables not in the dataset
  expect_error(
    cttab(
      x = c("AST", "BILI", "ALT", "MPG"),
      group = "ARM",
      data = df,
      row_split = "AVISIT",
      select = c("ALT" = "PERF == 1")
    ),
    "Variable MPG not in the dataset, please check!"
  )

  expect_error(
    stat_tab(
      vars = c("AST", "BILI", "ALT", "MPG"),
      group = "ARM",
      data = df,
      select = c("ALT" = "PERF == 1")
    ),
    "Variable MPG not in the dataset, please check!"
  )

  expect_error(
    stat_tab(
      vars = c("AST", "BILI", "ALT", "MPG"),
      group = "ARM",
      data = df,
      select = c("ALT" = "PERF == 1")
    ),
    "Variable MPG not in the dataset, please check!"
  )


  expect_error(
    cttab(~., data = df),
    "No variables provided to summarise"
  )

  expect_error(
    cttab(over_w + hi_age ~ 1 | AGE | SEX, data = df),
    "Invalid formula, multiple split provided."
  )

  expect_error(
    cttab(ARM | SEX + over_w ~ AGE, data = df),
    "Invalid formula, only"
  )

  expect_error(
    cttab(ARM + SEX ~ ., data = df),
    "Invalid formula, dot is not allowed."
  )
})

test_that("Check stat_tab", {
  dat <- expand.grid(
    id = 1:10, sex = c("Male", "Female"),
    treat = c(1, 2),
    dates = c(as.Date("2010-1-1"), as.Date("2015-1-1"))
  )
  dat$age <- runif(nrow(dat), 10, 50)
  dat$age[3] <- NA # Add a missing value
  dat$wt <- exp(rnorm(nrow(dat), log(70), 0.2))
  dat <- data.table::as.data.table(dat)

  var_lab(dat$sex) <- "Sex"
  var_lab(dat$age) <- "Age"
  var_lab(dat$treat) <- "Treatment Group"
  var_lab(dat$wt) <- "Weight"
  val_lab(dat$treat) <- c("Treated" = 1, "Placebo" = 2)


  tab1 <- stat_tab(c("age", "sex"),
    data = dat,
    total = FALSE,
    group = "treat"
  )

  expect_equal(ncol(tab1), 2)

  expect_error(
    stat_tab("dates",
      data = dat,
      group = "treat"
    ),
    "The class of variable"
  )

  dat$height <- dat$wt
  dat$height[dat$treat == 1] <- NA
  tab1 <- stat_tab("height",
    data = dat,
    group = "treat"
  )
  expect_equal(unname(tab1[2, ]), c("0", "40", "40"))

  dat$height <- NA

  expect_equal(
    dim(stat_tab("height",
      data = dat,
      group = "treat"
    )),
    c(3, 3)
  )

  dat$bmi <- NA
  val_lab(dat$bmi) <- c("Over" = 1, "Under" = 2)

  expect_null(stat_tab("bmi",
    data = dat,
    add_missing = F,
    group = "treat"
  ))

  expect_equal(
    dim(stat_tab("bmi",
      data = dat,
      group = "treat"
    )),
    c(2, 3)
  )

  dat$bmi <- factor(dat$bmi, levels = c(1, 2), labels = c("Over", "Under"))

  tab <- stat_tab("bmi",
    data = dat,
    group = "treat"
  )
  expect_equal(nrow(tab), 2)
  expect_equal(row.names(tab)[2], "Missing")

  expect_null(stat_tab("bmi",
    data = dat,
    group = "treat",
    add_missing = FALSE
  ))
})



test_that("Check for all missing", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  # All variables are missing
  df$SEX <- NA
  df$AGE <- NA
  X3 <- cttab(
    x = c("SEX", "AGE"),
    data = df,
    group = "ARM"
  )
  x3_out <- c("48", "", "48 (100%)", "", "0", "48 (100%)")
  names(x3_out) <- c(
    "Observation", "Sex", "Missing", "Age",
    "Valid Obs.", "Missing"
  )

  expect_identical(X3[, 1], x3_out)
  expect_no_error( write_table(X3, directory = test_path("Output/Core")))
})
