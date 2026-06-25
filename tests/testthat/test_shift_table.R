# shift_table is exercised on the pilot ECG / ECOG forms, mirroring the worked
# examples in demo/Progs/analysis.R (outputs 2.6 and 2.7). Snapshots pin the
# rendered tables so both the styling and every count are checked against a
# saved reference.

dt  <- read.csv(system.file("extdata", "pilotdata.csv", package = "cctu"),
                colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package = "cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package = "cctu"))
dt$subjid <- substr(dt$USUBJID, 8, 11)
dt <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)
options("cctu_digits_pct" = 1)

adsl <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
adsl$ARM <- to_factor(adsl$ARM)
arm_lk <- unique(adsl[, c("subjid", "ARM")])

ecg <- merge(extract_form(dt, "ECG", vars_keep = "subjid"), arm_lk, by = "subjid")
ecg$ECG <- factor(to_factor(ecg$ECGINT),
                  levels = c("Normal",
                             "Abnormal - not clinically significant",
                             "Abnormal - clinically significant"),
                  ordered = TRUE)

ecog <- merge(extract_form(dt, "ECOG", vars_keep = "subjid"), arm_lk, by = "subjid")
ecog$ECOG <- factor(to_factor(ecog$ECOG),
                    levels = c("Fully active",
                               "Restricted in strenuous activity",
                               "Ambulatory and self-care",
                               "Limited self-care",
                               "Completely disabled"),
                    ordered = TRUE)

test_that("shift_table ECG worst-shift across arms (analysis.R 2.6)", {
  X <- shift_table(ecg, value = "ECG", id_var = "subjid", visit = "form_visit",
                   bl_value = "SCREENING", col_groups = "ARM", pct = "none")
  expect_s3_class(X, "cttab")
  # col_groups only -> plain rows (no row-group banners, no styling).
  expect_true(all(attr(X, "row_style") == ""))
  expect_snapshot(print(X))
})

test_that("shift_table ECOG with arm row-group banners (analysis.R 2.7)", {
  X <- shift_table(ecog, value = "ECOG", id_var = "subjid", visit = "form_visit",
                   bl_value = "SCREENING", row_groups = "ARM", pct = "none")
  expect_s3_class(X, "cttab")
  # ARM (a factor) folds into bold banner rows above indented baseline rows.
  rs <- attr(X, "row_style")
  expect_true(any(grepl("bold", rs)))
  expect_true(any(grepl("indent", rs)))
  expect_snapshot(print(X))
})

test_that("shift_table row percentages, empty-margin and no-post dropping", {
  ecg2 <- ecg
  ecg2$ARM <- as.character(ecg2$ARM)   # character group -> non-factor grid path
  X <- shift_table(ecg2, value = "ECG", id_var = "subjid", visit = "form_visit",
                   bl_value = "SCREENING", col_groups = "ARM", pct = "row",
                   drop_empty = "both", drop_no_postbaseline = TRUE)
  # drop_no_postbaseline removes the "No post-baseline" category entirely.
  expect_false(any(grepl("No post-baseline", names(X))))
  expect_snapshot(print(X))
})

test_that("shift_table validates inputs and warns on absent baseline levels", {
  # Unknown grouping column.
  expect_error(
    shift_table(ecg, value = "ECG", id_var = "subjid", visit = "form_visit",
                bl_value = "SCREENING", row_groups = "NOPE"),
    "Columns not found")

  # `value` must be an ordered factor (levels best -> worst).
  ecg_chr <- ecg
  ecg_chr$ECG <- as.character(ecg_chr$ECG)
  expect_error(
    shift_table(ecg_chr, value = "ECG", id_var = "subjid", visit = "form_visit",
                bl_value = "SCREENING", col_groups = "ARM"),
    "ordered factor")

  # A bl_value level that never appears in `visit` warns but still builds.
  expect_warning(
    shift_table(ecg, value = "ECG", id_var = "subjid", visit = "form_visit",
                bl_value = c("SCREENING", "NOPE"), col_groups = "ARM"),
    "bl_value")

  # More than one baseline per subject x group.
  dup <- rbind(ecg, subset(ecg, form_visit == "SCREENING")[1, ])
  expect_error(
    shift_table(dup, value = "ECG", id_var = "subjid", visit = "form_visit",
                bl_value = "SCREENING", col_groups = "ARM"),
    "more than one")
})
