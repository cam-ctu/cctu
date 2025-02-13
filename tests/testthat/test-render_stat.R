# Render statistics

test_that("Numeric", {
  data(mtcars)
  x <- render_numeric(mtcars$mpg)
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Median [Min, Max]" = "19.2 [10.4, 33.9]"
  ))
  x <- rep(NA, 10)

  expect_identical(render_numeric(x), c(
    "Valid Obs." = "0",
    "Mean (SD)" = "",
    "Median [Min, Max]" = ""
  ))

  x <- render_numeric(mtcars$mpg,
    what = c(
      "Geo. Mean (Geo. CV%)" = "GMean (GCV)",
      "Geo. SD" = "GSD",
      "Median [IQR]" = "Median [IQR]"
    )
  )
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Geo. Mean (Geo. CV%)" = "19.3 (30.4%)",
    "Geo. SD" = "1.35",
    "Median [IQR]" = "19.2 [7.38]"
  ))
  # Quantile
  x <- render_numeric(mtcars$mpg,
    what = c("Median [Q1, Q3]" = "Median [Q1, Q3]")
  )

  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Median [Q1, Q3]" = "19.2 [15.4, 22.8]"
  ))

  x <- render_numeric(mtcars$mpg,
    what = c("GMean (GCV)",
      "Median [IQR]" = "Median [IQR]"
    )
  )
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "GMean (GCV)" = "19.3 (30.4%)",
    "Median [IQR]" = "19.2 [7.38]"
  ))
  expect_error(
    render_numeric(mtcars$mpg, what = "Mdian [IQR]"),
    "Statistics Mdian is not a valid statistics"
  )
})


test_that("Character", {
  data(mtcars)
  y <- factor(mtcars$am, levels = c(0, 1), labels = c("automatic", "manual"))
  y[1:10] <- NA

  expect_identical(render_cat(y), c(
    "automatic" = "12/22 (54.5%)",
    "manual" = "10/22 (45.5%)"
  ))

  y <- c(rep(T, 8), rep(F, 10))
  expect_identical(render_cat(y), c(
    "Yes" = "8/18 (44.4%)",
    "No" = "10/18 (55.6%)"
  ))
})
