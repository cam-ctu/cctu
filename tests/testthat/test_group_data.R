
test_that("group_data supports nested hierarchies and preserves data integrity", {
  # Setup: cyl (3 levels), am (2 levels)
  # Row Count: 3 (cyl headers) + 6 (cyl:am headers) + 32 (data) = 41
  dt_mtcars <- as.data.table(mtcars)
  groups <- c("cyl", "am")

  # 1. Test: Standard Header (No Shift)
  res_std <- group_data(dt_mtcars, groups = groups, shift = FALSE)

  expect_equal(nrow(res_std), 41)
  expect_equal(names(res_std)[1:2], groups)
  expect_true(is.numeric(res_std$mpg))
  expect_equal(attr(res_std, "is_header")[1], 1) # First row is Level 1 header
  expect_true(is.na(res_std$am[1]))      # Level 1 header has Level 2 as NA

  # Check data ordering (ignoring headers)
  dt_mtcars_tst <- dt_mtcars[order(cyl, am)]
  expect_identical(res_std$wt[attr(res_std, "is_header") == 0], dt_mtcars_tst$wt)

  # 2. Test: Shifted Header with keep_groups = FALSE
  res_shift <- group_data(dt_mtcars, groups = groups, shift = TRUE, keep_groups = FALSE)

  expect_false(any(groups %in% names(res_shift)))
  expect_equal(names(res_shift)[1], "mpg") # mpg is the target for labels
  expect_true(is.character(res_shift$mpg)) # Converted to char for labels
})

test_that("Shifting and nested indentation works correctly", {
  # Columns: Staff, Region, Dept
  # Groups: Region, Dept -> Target: Staff
  dt <- data.table(
    Staff = c("Alice", "Bob"),
    Region = c("North", "North"),
    Dept = c("Sales", "HR")
  )
  groups <- c("Region", "Dept")

  res <- group_data(dt, groups = groups, shift = TRUE, indent = TRUE)

  # Expected: North (L1), Sales (L2), Alice (Data), HR (L2), Bob (Data)
  expect_equal(nrow(res), 5)
  expect_equal(res$Staff[1], "North")
  expect_equal(res$Staff[2], "   HR")
  expect_equal(res$Staff[3], "      Bob")
  expect_equal(res$Staff[4], "   Sales")
  expect_equal(res$Staff[5], "      Alice")

  expect_equal(attr(res, "is_header"), c(1, 2, 0, 2, 0))
})

test_that("keep_groups parameter preserves column order", {
  dt <- data.table(Val = 10, G1 = "A", G2 = "B")
  groups <- c("G1", "G2")

  res <- group_data(dt, groups = groups, shift = TRUE, keep_groups = TRUE)

  # Groups must be first, then Val, then is_header
  expect_equal(names(res), c("G1", "G2", "Val"))
  expect_equal(res$Val[1], "A") # Shifted label
})

test_that("Function handles edge cases and errors", {
  # 1. Single group case
  dt_single <- data.table(Grp = c("A", "B"), Val = 1:2)
  res_single <- group_data(dt_single, groups = "Grp")
  expect_equal(nrow(res_single), 4) # 2 headers + 2 data

  # 2. Error: No other variables to shift into
  dt_no_vars <- data.table(G1 = "A")
  expect_error(group_data(dt_no_vars, groups = "G1", shift = TRUE),
               "At least one non-grouping variable is required")

  # 3. Error: Missing group column
  expect_error(group_data(dt_single, groups = "Missing"),
               "Grouping columns not found")
})

test_that("skip_na_label drops headers whose grouping value is NA", {
  dt <- data.table(G = c("A", NA, "B"), V = 1:3)

  # Default: skip_na_label = TRUE — NA group gets no header row
  res_skip <- group_data(dt, groups = "G")
  expect_equal(nrow(res_skip), 5)   # 2 headers ("A","B") + 3 data rows
  is_hdr <- attr(res_skip, "is_header")
  hdr_vals <- res_skip$G[is_hdr == 1]
  expect_false(any(is.na(hdr_vals)))

  # skip_na_label = FALSE — NA group gets a header row
  res_keep <- group_data(dt, groups = "G", skip_na_label = FALSE)
  expect_equal(nrow(res_keep), 6)   # 3 headers + 3 data rows
  is_hdr2 <- attr(res_keep, "is_header")
  hdr_vals2 <- res_keep$G[is_hdr2 == 1]
  expect_true(any(is.na(hdr_vals2)))
})

test_that("header_label_fn customises header strings", {
  dt <- data.table(Region = c("North", "South"), Val = 1:2)

  res <- group_data(dt, groups = "Region",
                    header_label_fn = function(lvl, val, grp) {
                      paste0("[L", lvl, "] ", val)
                    })

  is_hdr <- attr(res, "is_header")
  hdr_regions <- res$Region[is_hdr == 1]
  expect_equal(sort(hdr_regions), c("[L1] North", "[L1] South"))

  # With shift = TRUE the custom label ends up in the target column
  res_shift <- group_data(dt, groups = "Region", shift = TRUE,
                          header_label_fn = function(lvl, val, grp) {
                            paste0(grp, ":", val)
                          })
  is_hdr2 <- attr(res_shift, "is_header")
  hdr_vals <- res_shift$Val[is_hdr2 == 1]
  expect_equal(sort(hdr_vals), c("Region:North", "Region:South"))
})

test_that("skip_header_fn sees sub-data for the full group path", {
  dt <- data.table(
    G1 = c("A", "A", "B", "B"),
    G2 = c("x", "y", "x", "y"),
    Flag = c(TRUE, FALSE, FALSE, FALSE),
    V = 1:4
  )

  res <- group_data(
    dt,
    groups = c("G1", "G2"),
    skip_header_fn = function(level_idx, value, group_name, sub_data) {
      identical(group_name, "G2") && all(sub_data$Flag)
    }
  )

  is_hdr <- attr(res, "is_header")
  expect_false(any(is_hdr == 2 & res$G1 == "A" & res$G2 == "x"))
  expect_true(any(is_hdr == 2 & res$G1 == "B" & res$G2 == "x"))
})

test_that("preserve_attrs copies user attributes to output", {
  dt <- data.table(G = c("A", "B"), V = 1:2)
  attr(dt, "my_meta") <- list(source = "test", version = 2L)
  attr(dt, "row_split") <- "G"

  res <- group_data(dt, groups = "G", preserve_attrs = TRUE)
  expect_equal(attr(res, "my_meta"), list(source = "test", version = 2L))
  expect_equal(attr(res, "row_split"), "G")

  # preserve_attrs = FALSE — user attrs are not copied
  res_no <- group_data(dt, groups = "G", preserve_attrs = FALSE)
  expect_null(attr(res_no, "my_meta"))
  expect_null(attr(res_no, "row_split"))
})

