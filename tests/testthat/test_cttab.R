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

  df$group <- df$ARM
  expect_no_error(
    cttab(
      x = c("AGE", "SEX", "BMIBL"),
      group = "group",
      data = df,
      select = c("BMIBL" = "RACEN != 1")
    )
  )

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
  write_table(X, directory = tmp_dir, clean_up = FALSE)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab1.xml"),
    file.path(tmp_dir, "table_1.1.xml")
  ))

  # Make sure the write_table() method works with a data.frame and data.table input
  expect_s3_class(df, "data.table")
  expect_no_error(write_table(df,
                              directory = tmp_dir,
                              clean_up = FALSE))
  expect_snapshot_file(file.path(tmp_dir, "table_1.1.xml"),
                       name = "table_1.1.xml")
  expect_no_error(write_table(as.data.frame(df), directory = tmp_dir))
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
  ft1 <- cttab_format(X1)
  expect_equal(unname(as.character(ft1[[setdiff(names(ft1), "label")[1]]])),
               c("30/99 (30.3%)", "20/85 (23.5%)"))

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
  expect_identical(
    mis_rp$visit[mis_rp$subject_id == "1181, 1286, 1259"],
    "Baseline"
  )
  expect_identical(
    mis_rp$subject_id[mis_rp$visit == "Baseline" & mis_rp$variable == "AST"],
    "1160"
  )
  expect_identical(
    unique(mis_rp$form[mis_rp$variable == "inrange"]),
    "Derived"
  )

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir)

  expect_true(compare_file_text(
    test_path("ref", "table_ctab3.xml"),
    file.path(tmp_dir, "table_1.10.xml")
  ))
})

test_that("nest = 'var' flips the row hierarchy", {
  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

  X_split <- cttab(
    x = c("AST", "BILI"),
    group = "ARM",
    data = df,
    row_split = "AVISIT",
    nest = "split"
  )
  X_var <- cttab(
    x = c("AST", "BILI"),
    group = "ARM",
    data = df,
    row_split = "AVISIT",
    nest = "var"
  )

  expect_identical(attr(X_split, "nest"), "split")
  expect_identical(attr(X_var, "nest"), "var")

  m_split <- cttab_format(X_split)
  m_var   <- cttab_format(X_var)

  # Same set of rendered rows, just reordered.
  expect_setequal(m_split$label, m_var$label)

  # In split-mode the first banner row is the row_split header; in var-mode
  # it's the Observation banner (var-outer hierarchy).
  expect_match(m_split$label[1], "^Study Visit =")
  expect_equal(m_var$label[1], "Observation")

  # In var-mode each variable label sits as the outer banner.
  ast_idx <- match("Aspartate Aminotransferase (U/L)", m_var$label)
  expect_false(is.na(ast_idx))
  expect_equal(attr(m_var, "row_style")[ast_idx], "bold;bgcol;span")
})

test_that("Missing rows sort below other stats within a variable / group", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  X <- cttab(c("AGE", "BMIBL"), group = "ARM", data = df)
  # Within each (group level, variable), the Missing row appears after
  # the other stat rows. stat_tab assigns Stat_ID = length(stats) to the
  # Missing row, so the Missing-row indicator (Statistic == "Missing")
  # transitions FALSE -> TRUE at most once when iterating in Stat_ID order.
  X[Var_ID > 0L, expect_true(all(diff(as.integer(Statistic == "Missing")) >= 0L)),
    by = c("ARM", "Var_ID")]
})

test_that("as.data.frame.cttab returns a tidy frame", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

  X <- cttab(c("AGE", "SEX"), group = "ARM", data = df)
  tidy <- as.data.frame(X)

  expect_s3_class(tidy, "data.frame")
  expect_false(any(c("Group_ID", "Group_Label", "Stat_ID",
                     "Is_Missing", "Row_Style") %in% names(tidy)))
  # keeps the user-meaningful columns
  expect_true(all(c("Variable", "Statistic", "Value", "ARM") %in%
                    names(tidy)))
  expect_identical(attr(tidy, "group"), "ARM")
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
  expect_identical(mis_rp$subject_id[mis_rp$form == "Lab" &
                                       mis_rp$visit == "Baseline" &
                                       mis_rp$variable == "BILI"],
                   "1181, 1286, 1259")

  cctu_env$parent <- "test"
  write_table(X, directory = tmp_dir, clean_up = FALSE)

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

  # Two grouping levels (Treated/Placebo) become two data columns once formatted
  expect_equal(length(setdiff(names(cttab_format(tab1)), "label")), 2)

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
  # The 'Valid Obs.' stat row should read 0/40/40 across (Treated, Placebo, Total)
  ftab <- cttab_format(tab1)
  valid_row <- ftab[trimws(ftab$label) == "Valid Obs.", ]
  dc_valid <- setdiff(names(valid_row), "label")
  expect_equal(
    unname(unlist(valid_row[, dc_valid, drop = FALSE])),
    c("0", "40", "40")
  )

  dat$height <- NA
  # All-NA numeric: variable header + Valid Obs. (= "0") + Missing
  expect_equal(
    nrow(cttab_format(stat_tab("height", data = dat, group = "treat"))),
    3
  )
  expect_equal(
    length(setdiff(names(cttab_format(stat_tab("height", data = dat, group = "treat"))), "label")),
    3
  )

  dat$bmi <- NA
  val_lab(dat$bmi) <- c("Over" = 1, "Under" = 2)

  expect_null(stat_tab("bmi",
    data = dat,
    add_missing = F,
    group = "treat"
  ))

  # bmi is converted to factor via val_lab, all NA -> only Missing row + header
  expect_equal(nrow(cttab_format(stat_tab("bmi", data = dat, group = "treat"))), 2)
  expect_equal(
    length(setdiff(names(cttab_format(stat_tab("bmi", data = dat, group = "treat"))), "label")),
    3
  )

  dat$bmi <- factor(dat$bmi, levels = c(1, 2), labels = c("Over", "Under"))

  tab <- stat_tab("bmi",
    data = dat,
    group = "treat"
  )
  ftab <- cttab_format(tab)
  expect_equal(nrow(ftab), 2)
  expect_equal(trimws(ftab$label[2]), "Missing")

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
  # data.table preserves column type when assigning NA, so SEX stays
  # numeric+val_labels (rendered as a factor; all-NA categorical -> only
  # Missing row) and AGE stays numeric (Valid Obs. = 0 + Missing).
  x3_out <- c("48", "", "48 (100%)", "", "0", "48 (100%)")
  names(x3_out) <- c(
    "Observation", "Sex", "Missing",
    "Age", "Valid Obs.", "Missing"
  )

  ft3 <- cttab_format(X3)
  first_dc <- setdiff(names(ft3), "label")[1]
  expect_identical(setNames(as.character(ft3[[first_dc]]), ft3$label), x3_out)
  cctu_initialise(root = test_path("Output"))
  expect_no_error(write_table(X3, directory = test_path("Output/Core")))
})

test_that("print.cttab snapshot — numeric, factor, logical without row_split", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  df$hi_age <- df$AGE > 65
  var_lab(df$hi_age) <- "Age > 65"

  X <- cttab(
    x = c("AGE", "SEX", "hi_age"),
    group = "ARM",
    data = df
  )
  expect_snapshot(print(X))
})

test_that("print.cttab snapshot — numeric, factor, logical with row_split", {
  attach_pop("1.10")
  df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))
  df$hi_ast <- df$AST > 30
  var_lab(df$hi_ast) <- "High AST"

  X <- cttab(
    x = c("AST", "BILI", "hi_ast"),
    group = "ARM",
    data = df,
    row_split = "AVISIT"
  )
  expect_snapshot(print(X))
})


test_that("rbind.cttab stacks long-format tables and orders Total last", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  df$ARM <- factor(df$ARM)

  # Two cttab() calls with overlapping group levels.
  part1 <- cttab(c("AGE") ~ ARM, data = df, print_plot = FALSE)
  part2 <- cttab(c("SEX") ~ ARM, data = df, print_plot = FALSE)

  out <- rbind(part1, part2)

  expect_s3_class(out, "cttab")
  expect_identical(attr(out, "group"), "ARM")

  # Total stays at the end of the group factor levels.
  grp <- out[["ARM"]]
  expect_true(is.factor(grp))
  expect_identical(tail(levels(grp), 1), "Total")

  # Var_ID and Group_ID for part2 rows are offset strictly past part1's
  # max so each part keeps its own Observation row in the rendered output.
  expect_true(max(out$Var_ID) > max(part1$Var_ID))
  expect_true(max(out$Group_ID) > max(part1$Group_ID))

  # Total row count: rows from part1 + rows from part2 (no merging).
  expect_equal(nrow(out), nrow(part1) + nrow(part2))
})

test_that("rbind.cttab refuses to mix matrix and long-format inputs", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  long <- cttab(c("AGE") ~ ARM, data = df, print_plot = FALSE)
  formatted <- cttab_format(long)        # data.frame-with-label, not matrix
  # Force the matrix flavour to exercise the mixed-shape error.
  mat <- structure(
    matrix("x", nrow = 1, ncol = 1, dimnames = list("a", "v")),
    row_style = "",
    class = c("cttab", "matrix", "array")
  )
  expect_error(rbind(long, mat), "mix of matrix-format and long-format")
})

test_that("rbind.cttab on a single argument is a no-op", {
  attach_pop("1.1")
  df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
  one <- cttab(c("AGE") ~ ARM, data = df, print_plot = FALSE)
  expect_identical(rbind(one), one)
})
