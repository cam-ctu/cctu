# Small synthetic shift data: 6 subjects, baseline + one post-baseline visit,
# two parameters, two arms. `value` is an ordered factor (best -> worst).
shift_demo_data <- function() {
  dt <- data.table::data.table(
    USUBJID = rep(sprintf("S%02d", 1:6), each = 2),
    AVISIT  = rep(c("Baseline", "Week 4"), times = 6),
    PARAM   = rep(c("ALT", "AST"), each = 6),
    ARM     = rep(c("A", "B"), each = 2, length.out = 12),
    AVALC   = c("Normal", "High", "Normal", "Normal", "High", "High",
                "Normal", "High", "Low", "Normal", "Normal", "High")
  )
  dt[, AVALC := factor(AVALC, levels = c("Low", "Normal", "High"),
                       ordered = TRUE)]
  dt[]
}

test_that("shift_table styled wide uses group_data: bold banners, indented rows", {
  dt  <- shift_demo_data()
  out <- shift_table(dt, value = "AVALC", id = "USUBJID", visit = "AVISIT",
                     bl_value = "Baseline", row_groups = "PARAM",
                     col_groups = "ARM", pct = "none")

  expect_s3_class(out, "cttab")
  expect_equal(names(out)[1], "label")
  expect_length(attr(out, "row_style"), nrow(out))
  # The row-group column is folded into the label and dropped.
  expect_false("PARAM" %in% names(out))

  rs <- attr(out, "row_style")
  # One bold banner per row-group level (ALT, AST).
  banners <- which(trimws(out$label) %in% c("ALT", "AST"))
  expect_equal(length(banners), 2L)
  expect_true(all(grepl("bold", rs[banners])))
  # Every baseline (non-banner) row is indented, none bold.
  data_rows <- setdiff(seq_len(nrow(out)), banners)
  expect_true(all(grepl("indent", rs[data_rows])))
  expect_false(any(grepl("bold", rs[data_rows])))
})

test_that("shift_table banner rows are blank in the count columns", {
  dt  <- shift_demo_data()
  out <- shift_table(dt, value = "AVALC", id = "USUBJID", visit = "AVISIT",
                     bl_value = "Baseline", row_groups = "PARAM",
                     col_groups = "ARM", pct = "none")

  banners   <- which(trimws(out$label) %in% c("ALT", "AST"))
  data_cols <- setdiff(names(out), "label")
  banner_cells <- unlist(out[banners, data_cols])
  expect_true(all(banner_cells == ""))
})

test_that("shift_table with no row_groups returns plain (unstyled) rows", {
  dt  <- shift_demo_data()[PARAM == "ALT"]
  out <- shift_table(dt, value = "AVALC", id = "USUBJID", visit = "AVISIT",
                     bl_value = "Baseline", col_groups = "ARM",
                     pct = "none")

  expect_s3_class(out, "cttab")
  expect_equal(names(out)[1], "label")
  # No grouping -> no banners, no bold/indent: row_style is all empty.
  expect_true(all(attr(out, "row_style") == ""))
})

test_that("shift_table errors on more than one baseline per subject x group", {
  dt <- shift_demo_data()
  # Duplicate the ALT baseline row for S01 -> two baselines.
  dup <- dt[PARAM == "ALT" & USUBJID == "S01" & AVISIT == "Baseline"]
  dt2 <- rbind(dt, dup)
  expect_error(
    shift_table(dt2, value = "AVALC", id = "USUBJID", visit = "AVISIT",
                bl_value = "Baseline", row_groups = "PARAM"),
    "more than one"
  )
})

test_that("shift_table renders row percentages within baseline blocks", {
  dt  <- shift_demo_data()
  out <- shift_table(dt, value = "AVALC", id = "USUBJID", visit = "AVISIT",
                     bl_value = "Baseline", row_groups = "PARAM",
                     col_groups = "ARM", pct = "row")
  # Cells are "n (pct)" strings on data rows; banners stay blank.
  data_rows <- which(!trimws(out$label) %in% c("ALT", "AST"))
  cells <- unlist(out[data_rows, setdiff(names(out), "label")])
  expect_true(any(grepl("\\([0-9.]+\\)", cells)))
})
