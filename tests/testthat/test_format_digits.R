# Need to add more

test_that("Test for signif_pad", {
  x <- c(0.9001, 12356, 1.2, 1., 0.1, 0.00001, 1e5, 1.3467)
  expect_identical(
    signif_pad(x, digits = 3),
    c(
      "0.900", "12400", "1.20", "1.00", "0.100",
      "0.0000100", "100000", "1.35"
    )
  )

  expect_identical(
    signif_pad(x, digits = 3, round.integers = FALSE),
    c(
      "0.900", "12356", "1.20", "1.00", "0.100",
      "0.0000100", "100000", "1.35"
    )
  )
  x <- c(0.9001, Inf, -Inf, NA, NaN)
  expect_identical(
    signif_pad(x, digits = 3, round.integers = FALSE),
    c("0.900", "Inf", "-Inf", NA, "NaN")
  )

  expect_identical(
    signif_pad(exp(30), format = "g", digits = 3),
    "1.07e+13"
  )
})

test_that("Test for round_pad", {
  x <- c(0.9001, 12356, 1.2, 1., 0.1, 0.00001, 1e5, 1.3467)
  expect_identical(
    round_pad(x, digits = 2),
    c(
      "0.90", "12356.00", "1.20", "1.00", "0.10",
      "0.00", "100000.00", "1.35"
    )
  )

  x <- c(0, 0.9001, 156, 1.2, 1., 0.1, 0.00001, 0.00007, 0.0003)
  expect_identical(
    format_percent(x, digits = 2),
    c(
      "0%", "90.01%", "15600.00%", "120.00%", "100%",
      "10.00%", "0.00%", "0.01%", "0.03%"
    )
  )

  x <- c(NaN, 0.4, NA, 1)
  expect_equal(
    format_percent(x, digits = 2),
    c("", "40.00%", "", "100%")
  )

  x <- c(1, 2, 3, "a")
  expect_error(
    format_percent(x, digits = 2),
    "x must be numeric."
  )
})

test_that("Test for p-value", {
  pv <- c(-1, 0.00001, 0.00093, 0.001, 0.0042, 0.8999, 0.9, 1, NA)
  expect_identical(
    format_pval(pv),
    c(
      "<0.001", "<0.001", "<0.001", "0.001", "0.004",
      "0.900", "0.900", "1.000", NA
    )
  )
})

test_that("round_pad rounds negative numbers away from zero", {
  # The round5up nudge was a positive constant regardless of sign, so it pulled
  # negatives TOWARDS zero: round_pad(-0.135, 2) gave "-0.13" where even base
  # round() gives -0.14. Signed values (change-from-baseline) were biased.
  expect_identical(round_pad(-0.135, digits = 2), "-0.14")
  expect_identical(round_pad(0.135, digits = 2), "0.14")

  # Symmetric about zero.
  expect_identical(
    round_pad(-c(0.135, 0.245, 1.5), digits = 2),
    paste0("-", round_pad(c(0.135, 0.245, 1.5), digits = 2))
  )

  # -0.135 is not an exact tie (it is stored just beyond the halfway point), so
  # round5up must not change it and base round() is the reference.
  expect_identical(round_pad(-0.135, digits = 2),
                   formatC(round(-0.135, 2), digits = 2, format = "f",
                           flag = "0"))

  # Exact ties go AWAY from zero under round5up (the documented SAS/Excel
  # convention), which is where round_pad intentionally departs from base R.
  expect_identical(round_pad(c(-0.125, 0.125), digits = 2),
                   c("-0.13", "0.13"))
  # ... and with round5up = FALSE it defers to base R's go-to-even.
  expect_identical(round_pad(-0.125, digits = 2, round5up = FALSE), "-0.12")

  expect_identical(round_pad(c(1.5, NA), digits = 2), c("1.50", NA))
})

test_that("round_pad renders an exact zero as \"0\", distinct from a rounded-down value", {
  # "0.00" is ambiguous under round_pad: a small value rounds to it too. An
  # exact zero is "0"; a genuinely small value keeps the padded "0.00".
  expect_identical(round_pad(0, digits = 2), "0")
  expect_identical(round_pad(0.001, digits = 2), "0.00")
  expect_identical(round_pad(c(0, 1.5, 0.001), digits = 2),
                   c("0", "1.50", "0.00"))
  expect_identical(round_pad(c(0, NA), digits = 2), c("0", NA))
})

test_that("signif_pad leaves an exact zero as \"0\"", {
  # An exact 0 is rendered "0", not padded to "0.00": padding would read as a
  # small value that rounded down to zero, whereas "0" is unambiguously exact.
  expect_identical(signif_pad(c(5, 0, 0.5), digits = 3),
                   c("5.00", "0", "0.500"))
  expect_identical(signif_pad(0, digits = 3), "0")
  expect_identical(signif_pad(0, digits = 1), "0")
})

test_that("format_percent round-trips a zero-length input", {
  # `out <- rep("", 0)` was extended to length 1 by a length-1 logical index,
  # returning NA instead of character(0).
  expect_identical(format_percent(numeric(0)), character(0))
})
