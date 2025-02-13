test_that("Test rbind.cttab", {
  dat <- expand.grid(
    id = 1:10, sex = c("Male", "Female"),
    treat = c("Treated", "Placebo")
  )
  dat$age <- runif(nrow(dat), 10, 50)
  dat$age[3] <- NA # Add a missing value
  dat$wt <- exp(rnorm(nrow(dat), log(70), 0.2))
  #'
  var_lab(dat$sex) <- "Sex"
  var_lab(dat$age) <- "Age"
  var_lab(dat$treat) <- "Treatment Group"
  var_lab(dat$wt) <- "Weight"

  tab1 <- stat_tab("age",
    data = dat,
    group = "treat"
  )
  tab2 <- stat_tab("sex",
    data = dat,
    group = "treat"
  )
  tab3 <- stat_tab("wt",
    data = dat,
    group = "treat"
  )

  tmp <- rbind(tab1, tab2, tab3)

  expect_equal(
    attr(tmp, "position", exact = TRUE),
    c(
      attr(tab1, "position", exact = TRUE),
      attr(tab2, "position", exact = TRUE),
      attr(tab3, "position", exact = TRUE)
    )
  )

  expect_s3_class(tmp, "cttab")

  expect_error(
    rbind(tab1, mtcars),
    "Only cttab class is supportted."
  )
})

test_that("Test all_is_numeric", {
  expect_true(all_is_numeric(c("1", "1.2", "3", "")))
  expect_true(all_is_numeric(c("1", "1.2", NA)))
  expect_false(all_is_numeric(c("1", "1.2", "3a")))
})
