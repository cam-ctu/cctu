# Render statistics

test_that("Numeric", {
  data(mtcars)
  x <- render_numeric(mtcars$mpg)
  expect_identical(x, c("Valid Obs." = "32",
                        "Mean (SD)"  = "20.1 (6.03)",
                        "Median [Min, Max]" = "19.2 [10.4, 33.9]"))
  x <- rep(NA, 10)

  expect_identical(render_numeric(x), c("Valid Obs." = "0",
                                        "Mean (SD)"  = "",
                                        "Median [Min, Max]" = ""))

})


test_that("Character", {
  data(mtcars)
  y <- factor(mtcars$am, levels = c(0, 1),labels=c("automatic", "manual"))
  y[1:10] <- NA

  expect_identical(render_cat(y), c("automatic" = "12/22 (54.5%)",
                                    "manual"  = "10/22 (45.5%)"))

  y <- c(rep(T, 8), rep(F, 10))
  expect_identical(render_cat(y), c("Yes" = "8/18 (44.4%)",
                                    "No"  = "10/18 (55.6%)"))

})


