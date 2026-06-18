utils::globalVariables(c(
  ".",
  # cttab metadata columns referenced as bare symbols in NSE contexts across
  # several cttab files (cttab.R, cttab_format.R, cttab_helpers.R, ...)
  "Group_ID", "Var_ID", "Stat_ID", "Group_Label", "Variable",
  "Statistic", "Row_Style", "Value",
  # data.table count column shared by several summarisers (group_data, ...)
  "n"
  # NOTE: function-specific NSE column names are now bound locally as `NULL`
  # at the top of the function that uses them (the data.table-recommended
  # idiom), rather than declared here. See e.g. group_data(),
  # cttab_format()'s helpers, report_missing(), stat_tab(), shift_table()
  # and ae_summary().
))
# may well have to add "cctu_env" to the line above. But I can't get
# devtools::check() to work past installing,
# so impossible to check at the moment.
cctu_env <- new.env(parent = emptyenv())
cctu_env$number <- "0"
cctu_env$sumby_count <- 0
cctu_env$nested_run_batch <- FALSE

# Missing data report - `missing_report_chunks` is a list of per-cttab()
# data.frames that get_missing_report() consolidates lazily.
cctu_env$missing_report_data <- setNames(
  data.frame(matrix(ncol = 8, nrow = 0)),
  c(
    "form", "visit_var", "visit_label",
    "visit", "variable", "label", "missing_pct",
    "subject_id"
  )
)
cctu_env$missing_report_chunks <- list()
# Setup DLU file
cctu_env$dlu <- NULL

# .reserved <- character(0)

# This is done to create a binding within the package namespace, so that
# it can be mocked as per
# https://testthat.r-lib.org/reference/local_mocked_bindings.html#base-functions
# in the testing
# Is this to override the date stamp in the report outputs, to allow testing
# and direct comparison of outputs in CI/CD.
Sys.time <- NULL # nolint: object_name_linter
