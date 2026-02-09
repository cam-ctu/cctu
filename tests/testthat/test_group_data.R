

test_that("group_data preserves data integrity and types", {
  # 1. Setup
  dt_mtcars <- mtcars
  groups <- c("cyl", "am")

  # Test: Standard Header (No Shift)
  res_std <- group_data(dt_mtcars, groups = groups, shift = FALSE)

  # - Check Row Count: Original 32 + 6 unique (cyl, am) combos = 38
  expect_equal(nrow(res_std), 41)
  # - Check Column Order: groups must be first
  expect_equal(names(res_std)[1:2], groups)
  # - Check Type Preservation: 'mpg' must remain numeric
  expect_true(is.numeric(res_std$mpg))

  # - Check Variable Order Preservation: 'wt' must remain the same
  dt_mtcars_tst <- dt_mtcars[order(dt_mtcars$cyl, dt_mtcars$am), ]
  expect_identical(res_std$wt[!is.na(res_std$wt)], dt_mtcars_tst$wt)

  # Test: Shifted Header
  res_shift <- group_data(dt_mtcars, groups = groups, shift = TRUE, indent = TRUE)

  # - Check Column Removal: original groups should be dropped
  expect_false(any(groups %in% names(res_shift)))
  # - Check Target Column: 'mpg' was the first non-group, should be character now
  expect_true(is.character(res_shift$mpg))
  # - Check Indentation: Data rows (index 1) should have spaces
  # For the first data row of the first group:
  expect_match(res_shift[2, mpg], "^   [0-9]")

})


test_that("Multi-column grouping works correctly", {
  dt <- data.table(G1 = c("X", "X"), G2 = c(1, 2), Val = c(10, 20))
  res <- group_data(dt, groups = c("G1", "G2"))

  # 2 unique combinations + 2 data rows = 4
  expect_equal(nrow(res), 5)
  # Ensure both grouping columns are preserved when shift = FALSE
  expect_true(all(c("G1", "G2") %in% names(res)))
})

test_that("Function handles edge case with no other variables to shift into", {
  dt <- data.table(Grp = "A")
  # Expect an error if shift = TRUE but no other columns exist
  expect_error(group_data(dt, groups = "Grp"),
               "At least one non-grouping variable is required")
})

test_that("group_data supports nested hierarchies", {
  # 1. Setup mtcars
  # cyl has 3 levels (4, 6, 8)
  # am has 2 levels (0, 1)
  # Total combinations (cyl, am) = 6
  dt_mtcars <- as.data.table(mtcars)
  groups <- c("cyl", "am")

  # Nested logic:
  # Headers Level 1 (cyl): 3 rows
  # Headers Level 2 (cyl+am): 6 rows
  # Data: 32 rows
  # Expected total: 32 + 3 + 6 = 41

  res_std <- group_data(dt_mtcars, groups = groups, shift = FALSE)
  expect_equal(nrow(res_std), 41)

  # Check that Level 1 headers have NA in the 2nd group column
  # In standard mode, Level 1 header for cyl=4 should have am=NA
  level1_cyl4 <- res_std[cyl == 4, ][1]
  expect_true(is.na(level1_cyl4$am))
})

test_that("Shifting and nested indentation works correctly", {
  dt <- data.table(
    Region = c("North", "North"),
    Dept = c("Sales", "HR"),
    Staff = c("Alice", "Bob"),
    Sales = c(10, 20)
  )
  groups <- c("Region", "Dept")

  res <- group_data(dt, groups = groups, shift = TRUE, indent = TRUE)

  expect_equal(nrow(res), 5) # 1 Region + 2 Depts + 2 Data
  expect_equal(res$Staff[1], "North")
  expect_equal(res$Staff[2], "   HR")
  expect_equal(res$Staff[3], "      Bob")
  expect_equal(res$Staff[4], "   Sales")
  expect_equal(res$Staff[5], "      Alice")
})

test_that("Data types are preserved in nested output", {
  dt <- data.table(G1 = "A", G2 = "B", Val = 100)
  res <- group_data(dt, groups = c("G1", "G2"), shift = FALSE)

  # Val should still be numeric even with NAs in header rows
  expect_true(is.numeric(res$Val))
  # Row 1 (G1 header), Row 2 (G2 header), Row 3 (Data)
  expect_equal(res$Val[3], 100)
  expect_true(all(is.na(res$Val[1:2])))
})

test_that("Function handles edge cases", {
  # Single group (should behave like the old version)
  dt <- data.table(Grp = c("A", "A", "B"), Val = 1:3)
  res <- group_data(dt, groups = "Grp")
  expect_equal(nrow(res), 5) # 2 headers + 3 data

  # Error when no columns to shift into
  dt_small <- data.table(G1 = "A", G2 = "B")
  expect_error(group_data(dt_small, groups = c("G1", "G2"), shift = TRUE),
               "At least one non-grouping variable is required")
})

