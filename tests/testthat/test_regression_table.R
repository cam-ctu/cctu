

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
  expect_warning(output_exp <- regression_table(fit, trans=exp))
  expect_true("Ratio" %in% names(output_exp) )

  est <- output[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
  expect_true( compare_with_rounding(est[1:18], x_ref[,2]) %>% all )
  se <- output[,2] %>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
  expect_true( compare_with_rounding(se[1:18],x_ref[,3]) %>% all)
  expect_true( compare_with_rounding(output[1:18,4],x_ref[,5]) %>% all )
}
)



# chapter 20 for coxph : doesn't provide any simple tables of HRs - uses normagrams and plots
# Use the vignette from teh survival package directly
test_that("coxph regression",{
  library(survival)
  load(file=test_path("fixtures","survival.Rdata"))
  cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)
  output <- regression_table(cfit1, digits=5)
  hr <- output[1:3,3]
  expect_true(compare_with_rounding(hr, X[,"exp(coef)"]) %>% all)
  loghr <- output[1:3,2]%>% gsub( "\\(.*\\)","",. ) %>% trimws()
  expect_true(compare_with_rounding(loghr, X[,"coef"]) %>% all)
  se <- output[1:3,2]%>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
  compare_with_rounding(se, X[,"se(coef)"]) %>% all %>%  expect_true
  compare_with_rounding(output[1:3, 5],X[,"p"]) %>% all %>% expect_true
}
)


# lme
test_that("lme",{
  fm2 <- nlme::lme(Reaction~Days, random=~Days|Subject, data=lme4::sleepstudy)
  expect_warning(output <- regression_table(fm2, digits=4),"The variance-correlation structure is not included as it is too general")
  load(test_path("fixtures","lme.Rdata"))
  est <- output[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
  compare_with_rounding(est[1:2], X[,2]) %>% all %>% expect_true
  se <- output[,2] %>% gsub( ".*\\((.*)\\)","\\1",. ) %>% trimws()
  compare_with_rounding(se[1:2],X[,3]) %>% all %>% expect_true
  compare_with_rounding(est[5:7], Y[c(1,2,4),"vcov"]) %>% all %>% expect_true
  compare_with_rounding(output[5:7,3], Y[c(1,2,4),"sdcor"]) %>% all %>% expect_true
  # maybe some rounding and then padding with zeroes going on
  compare_with_rounding(output[6,4] %>% as.numeric, Y[3,"sdcor"]) %>% expect_true
})






# Need a test of gee
test_that("gee",{
load( file=test_path("fixtures", "linear.Rdata"))
fm0 <- lm(fat ~ rms::rcs(age,4) + log(height) + log(abdomen),
          data = bodyfat)
library(gee)
fm <- gee( fat ~ rms::rcs(age,4) + log(height) + log(abdomen),
    data = bodyfat, id=1:nrow(bodyfat), corstr = "exchangeable")
lin_mod <- regression_table(fm0)
expect_warning( gee_mod <- regression_table(fm), "The variance-correlation structure is not included as it is too general")
est1 <- lin_mod[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
est2 <- gee_mod[,2] %>% gsub( "\\(.*\\)","",. ) %>% trimws()
compare_with_rounding(est1[1:6], est2[1:6]) %>% all %>% expect_true
})


test_that("poisson",{
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  data.frame(treatment, outcome, counts) # showing data
  glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
  X <- regression_table(glm.D93)
  expect_true( "RR" %in% names(X))
})
