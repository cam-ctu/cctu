

# Test of logistic regression
context("Testing the regression table functions")

test_that("logisitic compare est, se and pvalue",{
load(file=test_path("fixtures","logistic.Rdata"))
  fit <- glm(response~sex, family=binomial, data=sex.age.response)
  output <- regression_table(fit, digits=4)

est <- output[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
expect_true( compare_with_rounding(est[1:2], x_ref[,2]) %>% all )
se <- output[,2] %>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
expect_true( compare_with_rounding(se[1:2],x_ref[,3]) %>% all)
expect_true( compare_with_rounding(output[2,5],x_ref[2,5]) )
})

# test of linear regression

#
#rms Suggests in DESCRIPTION,  and then renv looks after me with renv::install()


test_that("linear compare est, se and pvalue",{
  load( file=test_path("fixtures", "linear.Rdata"))

  fit <- lm( fat ~ rms::rcs(age,4) + log(height) + log(abdomen),
             data = bodyfat)
  output <- regression_table(fit, digits = 4)
  est <- output[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
  expect_true( compare_with_rounding(est[1:6], x_ref[,2]) %>% all )
  se <- output[,2] %>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
  # there is a rounding error on teh website for the last value
  se[6] <- substr(se[6],1,6)
  expect_true( compare_with_rounding(se[1:6],x_ref[,3]) %>% all)
  expect_true( compare_with_rounding(output[1:6,4],x_ref[,5]) %>% all )
})


# chapter 7 for gls
test_that("gls compare est, se and pvalue",{
  load(file=test_path("fixtures", "gls.Rdata"))
  expect_warning(fit <- nlme::gls( twstrs ~ treat * rms::rcs(week, 3) + rms::rcs(twstrs0, 3) +
                      rms::rcs(age, 4) * sex, data = both,
                    correlation = nlme::corCAR1(form = ~week |uid)),
   "3 knots requested with 5 unique values of x.  knots set to 3 interior values."
  )

  expect_warning(
    output <- regression_table(fit, digits=4),
     "The variance-correlation structure is not included as it is too general"
  )


  est <- output[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
  expect_true( compare_with_rounding(est[1:18], x_ref[,2]) %>% all )
  se <- output[,2] %>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
  expect_true( compare_with_rounding(se[1:18],x_ref[,3]) %>% all)
  expect_true( compare_with_rounding(output[1:18,4],x_ref[,5]) %>% all )
}
)



# chapter 20 for coxph : doesn't provide any simple tables of HRs - uses normagrams and plots


# anything for lme??

