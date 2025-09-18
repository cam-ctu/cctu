test_that("produce different flavour of plot", {


operating_system <-    Sys.info()["sysname"]

  local_edition(3)
  library(survival)
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  fig0 <- km_ggplot(fit)
  vdiffr::expect_doppelganger("basic", fig0)
  fig1 <- km_ggplot(fit, timeby = 500)
  vdiffr::expect_doppelganger("basic_more_ticks", fig1)
  fig2 <- km_ggplot(fit, pval = TRUE)
  vdiffr::expect_doppelganger("pvalue", fig2)
  options("cctu_p_digits" = 10)
  fig3 <- km_ggplot(fit, pval = TRUE)
  vdiffr::expect_doppelganger("pvalue_digits_options", fig3)
  options("cctu_p_digits" = NULL)
  fig4 <- km_ggplot(fit, pval = TRUE)
  vdiffr::expect_doppelganger("pvalue", fig4)
  # checking that fig2 and fig4 are the same

  fit0 <- survfit(Surv(time, status) ~ 1, data = colon)
  fig5 <- km_ggplot(fit0)
  vdiffr::expect_doppelganger("single", fig5)
  expect_error(km_ggplot(fit0, pval = TRUE), "No groups to test")

  fit_sex <- survfit(Surv(time, status) ~ rx + strata(sex), data = colon)
  fig6 <- km_ggplot(fit_sex)
  vdiffr::expect_doppelganger("strata", fig6)
  expect_warning(km_ggplot(fit_sex, ystratalabs = names(fit_sex$strata)), "deprecated")

  fig7 <- km_ggplot(fit,
    xlabs = "Days", ylabs = "Survival", strata_labs = c("A", "B", "C"),
    ylims = c(0.5, 1), xlims = c(0, 2000)
  )
  vdiffr::expect_doppelganger("labs", fig7)
})
