test_that("Test rbind.cttab", {
  dat <- expand.grid(
    id = 1:10, sex = c("Male", "Female"),
    treat = c("Treated", "Placebo")
  )
  dat$age <- runif(nrow(dat), 10, 50)
  dat$age[3] <- NA # Add a missing value
  dat$wt <- exp(rnorm(nrow(dat), log(70), 0.2))

  var_lab(dat$sex) <- "Sex"
  var_lab(dat$age) <- "Age"
  var_lab(dat$treat) <- "Treatment Group"
  var_lab(dat$wt) <- "Weight"

  options(cctu_print_plot = FALSE)

  tab1 <- cttab("age",   data = dat, group = "treat")
  tab2 <- cttab("sex",   data = dat, group = "treat")
  tab3 <- cttab("wt",    data = dat, group = "treat")

  tmp <- rbind(tab1, tab2, tab3)

  # Long-format stays long-format
  expect_s3_class(tmp, "cttab")
  expect_s3_class(tmp, "data.table")
  expect_equal(nrow(tmp), nrow(tab1) + nrow(tab2) + nrow(tab3))

  # The bound parts get distinct Var_IDs so they sort in input order
  expect_true(all(diff(unique(tmp$Var_ID[tmp$Var_ID > 0L])) > 0L))

  # Once formatted, the rendered row_style vector concatenates the parts'
  # rendered styles in order.
  expect_equal(
    attr(cttab_format(tmp), "row_style", exact = TRUE),
    c(
      attr(cttab_format(tab1), "row_style", exact = TRUE),
      attr(cttab_format(tab2), "row_style", exact = TRUE),
      attr(cttab_format(tab3), "row_style", exact = TRUE)
    )
  )

  expect_error(
    rbind(tab1, mtcars),
    "Only cttab class is supported."
  )

  # Mismatched grouping -> error
  tab_no_grp <- cttab("age", data = dat)
  expect_error(rbind(tab1, tab_no_grp), "different `group` variables")
})

test_that("Test all_is_numeric", {
  expect_true(all_is_numeric(c("1", "1.2", "3", "")))
  expect_true(all_is_numeric(c("1", "1.2", NA)))
  expect_false(all_is_numeric(c("1", "1.2", "3a")))
})
