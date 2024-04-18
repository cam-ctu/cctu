

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

# chapter 20 for coxph


