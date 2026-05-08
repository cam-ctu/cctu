
test_that("group_data inserts standard headers for nested groups", {
  dt_mtcars <- as.data.table(mtcars)
  groups <- c("cyl", "am")

  res <- group_data(dt_mtcars, groups = groups)

  # Group columns are placed at the front.
  expect_equal(names(res)[1:2], groups)
  # Numeric columns retain their type (no label-shifting happened).
  expect_true(is.numeric(res$mpg))

  # All 32 mtcars rows are still present (header rows have NA in the
  # non-grouping columns; data rows do not).
  data_rows <- res[!is.na(mpg), ]
  expect_equal(nrow(data_rows), nrow(dt_mtcars))
  # Each data row's (cyl, am) is preserved.
  expect_setequal(
    paste(data_rows$cyl, data_rows$am),
    paste(dt_mtcars$cyl, dt_mtcars$am)
  )

  # Header rows: one per unique cyl plus one per unique (cyl, am).
  hdr <- res[is.na(mpg), ]
  uniq_l1 <- nrow(unique(dt_mtcars[, "cyl"]))
  uniq_l2 <- nrow(unique(dt_mtcars[, c("cyl", "am")]))
  expect_equal(nrow(hdr), uniq_l1 + uniq_l2)
})

test_that("shift_to moves headers into the target column with indentation", {
  dt <- data.table(
    Staff  = c("Alice", "Bob"),
    Region = c("North", "North"),
    Dept   = c("Sales", "HR")
  )

  res <- group_data(dt, groups = c("Region", "Dept"),
                    shift_to = "Staff", indent = TRUE)

  # Expected layout (one header per group level + data rows underneath):
  #   "North"             (L1 header)
  #   "   Sales"          (L2 header)
  #   "      Alice"       (data, indented past the deepest header)
  #   "   HR"             (L2 header)
  #   "      Bob"         (data)
  expect_equal(nrow(res), 5)
  expect_equal(res$Staff[1], "North")
  expect_equal(res$Staff[2], "   Sales")
  expect_equal(res$Staff[3], "      Alice")
  expect_equal(res$Staff[4], "   HR")
  expect_equal(res$Staff[5], "      Bob")

  # Group columns are dropped when shift_to is set (their information has
  # been folded into the target column).
  expect_false(any(c("Region", "Dept") %in% names(res)))
})

test_that("shift_to without indent leaves header labels unprefixed", {
  dt <- data.table(Val = c("a", "b"), G = c("X", "Y"))
  res <- group_data(dt, groups = "G", shift_to = "Val", indent = FALSE)

  # Two header rows ("X", "Y") followed by their data rows ("a", "b").
  expect_equal(nrow(res), 4)
  expect_equal(res$Val, c("X", "a", "Y", "b"))
})

test_that("carry propagates per-group-unique values to header rows", {
  # cttab uses carry=c("Group_ID", "Var_ID", ...) so banner / variable
  # header rows can keep numeric ids that downstream styling logic reads.
  dt <- data.table(
    G       = c("A", "A", "B", "B"),
    V       = 1:4,
    GroupId = c(1L, 1L, 2L, 2L),
    VarId   = c(10L, 10L, 20L, 20L)
  )

  res <- group_data(dt, groups = "G", shift_to = "V",
                   carry = c("GroupId", "VarId"))

  # Header rows: V holds the group label, GroupId/VarId are carried from
  # the underlying group's unique value rather than being NA.
  hdr <- res[V %in% c("A", "B"), ]
  expect_equal(hdr$GroupId, c(1L, 2L))
  expect_equal(hdr$VarId,   c(10L, 20L))
})

test_that("carry rejects columns whose group values are not unique", {
  dt <- data.table(G = c("A", "A"), V = 1:2, Flag = c(TRUE, FALSE))
  expect_error(
    group_data(dt, groups = "G", shift_to = "V", carry = "Flag"),
    "carry column 'Flag' has non-unique values"
  )
})

test_that("group_data validates inputs", {
  dt_single <- data.table(Grp = c("A", "B"), Val = 1:2)
  # Single grouping column produces 2 headers + 2 data rows.
  res_single <- group_data(dt_single, groups = "Grp")
  expect_equal(nrow(res_single), 4)

  # No non-grouping column to hold shifted labels.
  dt_no_vars <- data.table(G1 = c("A", "B"))
  expect_error(group_data(dt_no_vars, groups = "G1", shift_to = "G1"),
               "shift_to column cannot be a grouping column")

  # Missing grouping column.
  expect_error(group_data(dt_single, groups = "Missing"),
               "Grouping columns not found")

  # Missing shift_to column.
  expect_error(group_data(dt_single, groups = "Grp", shift_to = "Nope"),
               "shift_to column not found")

  # Missing carry column.
  expect_error(group_data(dt_single, groups = "Grp", carry = "Nope"),
               "carry columns not found")
})
