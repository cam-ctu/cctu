
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

