# ae_summary is exercised on the pilot AE form, mirroring the worked example in
# demo/Progs/analysis.R (output 2.5). Snapshots pin the rendered tables so the
# styling and every count are checked against a saved reference.

dt  <- read.csv(system.file("extdata", "pilotdata.csv", package = "cctu"),
                colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package = "cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package = "cctu"))
dt$subjid <- substr(dt$USUBJID, 8, 11)
dt <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)
options("cctu_digits_pct" = 1)

adsl <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
adsl$ARM <- to_factor(adsl$ARM)

ae <- extract_form(dt, "AE", vars_keep = c("subjid"))
ae$SOC   <- to_factor(ae$AESOC)
ae$GRADE <- to_factor(ae$AETOXGR)

test_that("ae_summary SOC/PT worst-grade AE table (analysis.R 2.5)", {
  X <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                  term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                  any_label = "Participants with any adverse event",
                  min_pct = 5)
  expect_s3_class(X, "cttab")
  expect_equal(names(X)[1], "label")
  expect_snapshot(print(X))
})

test_that("ae_summary horizontal grade columns and high-grade collapsing", {
  horiz <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                      term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                      grade_layout = "horizontal", min_pct = 15)
  expect_snapshot(print(horiz))

  high <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                     term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                     grade_report = "high", grade_high = 3, min_pct = 15)
  expect_snapshot(print(high))
})

test_that("ae_summary flat (no class) any-grade table", {
  flat <- ae_summary(ae, adsl, id_var = "subjid", term_var = "AEPT",
                     trt_var = "ARM", min_pct = 10)
  expect_false("SOC" %in% names(flat))
  expect_snapshot(print(flat))
})

test_that("ae_summary column/sort options: no-treatment, alpha, explicit grades", {
  # No treatment variable -> a single Total column.
  tot <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                    term_var = "AEPT", grade_var = "GRADE", min_pct = 10)
  expect_equal(setdiff(names(tot), "label"), "Total")

  # class_sort changes the class-banner ordering ("freq" by subject count vs the
  # SOC factor-level order used by "alpha"), exercising both branches.
  banners <- function(x) trimws(x$label)[attr(x, "row_style") == "bold"][-1]
  freq  <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                      term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                      class_sort = "freq", min_pct = 10)
  alpha <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                      term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                      class_sort = "alpha", min_pct = 10)
  expect_false(identical(banners(freq), banners(alpha)))

  # A caller-supplied grade order is accepted.
  g <- ae_summary(ae, adsl, id_var = "subjid", class_var = "SOC",
                  term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                  grades = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
                  min_pct = 10)
  expect_s3_class(g, "cttab")
})

test_that("ae_summary warns and errors on non-numeric grade labels", {
  ae_sev <- data.frame(
    subjid = c("1001", "1002", "1003"), SOC = "Skin", AEPT = "Rash",
    GRADE  = c("Mild", "Severe", "Moderate"), stringsAsFactors = FALSE)
  adsl_sev <- data.frame(subjid = c("1001", "1002", "1003"),
                         ARM = factor(c("A", "B", "A")), stringsAsFactors = FALSE)

  # Severity order can't be parsed -> worst-grade counting warns.
  expect_warning(
    ae_summary(ae_sev, adsl_sev, id_var = "subjid", class_var = "SOC",
               term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE"),
    "alphabetical")

  # grade_report = "high" cannot collapse non-numeric grades and errors.
  expect_error(
    suppressWarnings(
      ae_summary(ae_sev, adsl_sev, id_var = "subjid", class_var = "SOC",
                 term_var = "AEPT", trt_var = "ARM", grade_var = "GRADE",
                 grade_report = "high")),
    "numeric grade labels")
})
