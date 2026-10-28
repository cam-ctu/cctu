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

test_that("a statistic name nested in a longer one is not substituted inside it", {
  # gsub() on the bare name replaced "Mean" inside "GMean", so "Mean (GMean)"
  # rendered as "20.1 (G20.1)" - a plausible-looking but wrong number. Names
  # must match as whole words only, and the order in the template must not
  # matter ("GMean (Mean)" happened to work; "Mean (GMean)" did not).
  data(mtcars)
  r <- function(what) render_numeric(mtcars$mpg, what = what)[[what]]

  expect_identical(r("Mean (GMean)"), "20.1 (19.3)")
  expect_identical(r("GMean (Mean)"), "19.3 (20.1)")
  expect_identical(r("CV (GCV)"), "30.0% (30.4%)")
  expect_identical(r("SD (GSD)"), "6.03 (1.35)")
  # 'N' sits inside 'MEAN' when the user spells the statistic in upper case.
  expect_identical(r("N (MEAN)"), "32 (20.1)")
})

test_that("an undefined statistic shows NA only inside a composite; alone it is blank", {
  # sd() of one observation is NA. The hard-coded row guarded MEAN but not SD
  # and printed a literal "5.00 (NA)", while what = "Mean (SD)" rendered ""
  # for the very same input - the two paths disagreed. Both now read
  # "5.00 (NA)". "NA" (not "-") avoids being misread as a negative sign.
  one <- render_numeric(5)
  expect_identical(one[["Mean (SD)"]], "5.00 (NA)")
  expect_identical(render_numeric(5, what = "Mean (SD)")[["Mean (SD)"]],
                   "5.00 (NA)")

  # "NA" appears only next to a value that WAS computed. A statistic that is
  # undefined on its own renders "" -- the SAME as when there is no data at
  # all -- so a given stat looks consistent regardless of WHY it is missing.
  # (SD of n=1, CV of a zero mean, GMean of non-positive data.)
  expect_identical(render_numeric(5, what = "SD")[["SD"]], "")
  expect_identical(render_numeric(rep(NA_real_, 3), what = "SD")[["SD"]], "")
  expect_identical(render_numeric(c(0, 0, 0), what = "CV")[["CV"]], "")
  expect_identical(render_numeric(c(-1, 2, 3), what = "GMean")[["GMean"]], "")

  # Nothing available at all (no data) -> blank cell, not "NA [NA, NA]".
  allna <- render_numeric(rep(NA_real_, 3))
  expect_identical(allna[["Mean (SD)"]], "")
  expect_identical(allna[["Median [Min, Max]"]], "")
  expect_identical(allna[["Valid Obs."]], "0")

  # Composite with at least one value computed -> show it, NA the rest.
  expect_identical(
    render_numeric(c(1, 2, NA), what = "Median [Min, Max]")[["Median [Min, Max]"]],
    "1.50 [1.00, 2.00]"
  )
  expect_identical(
    render_numeric(c(-1, 2, 3), what = "GMean (Mean)")[["GMean (Mean)"]],
    "NA (1.33)"
  )
})

test_that("Inf is a real value and survives rendering", {
  # The old guard blanked any rendered string containing no digit, which
  # silently deleted Inf/-Inf (signif_pad deliberately preserves them).
  expect_identical(render_numeric(c(1, 2, Inf), what = "Max")[["Max"]], "Inf")
  expect_identical(render_numeric(c(-Inf, 1, 2), what = "Min")[["Min"]], "-Inf")
})

test_that("CV is unavailable when the mean is zero", {
  # sd/0 is Inf and 0/0 is NaN; these reached the table as "Inf%" and "NaN".
  # Mean of each dataset is exactly 0, which renders "0" (not "0.00").
  expect_identical(
    render_numeric(c(-5, 5, -3, 3), what = "Mean (CV)")[["Mean (CV)"]],
    "0 (NA)"
  )
  expect_identical(
    render_numeric(c(0, 0, 0), what = "Mean (CV)")[["Mean (CV)"]],
    "0 (NA)"
  )
  # A non-zero mean still reports CV.
  expect_match(render_numeric(c(1, 2, 3), what = "CV")[["CV"]], "%$")
})

test_that("render_cat mirrors base table() and never invents a 'Missing' level", {
  # The ordinary factor: missing VALUES are dropped from the rows and from the
  # PCTnoNA denominator, exactly like table(useNA = "no"). Missingness itself is
  # reported separately by stat_tab, not by cat_stat.
  g <- factor(c("a", "a", "b", NA, NA), levels = c("a", "b"))
  expect_identical(names(render_cat(g)), names(table(g, useNA = "no")))
  expect_identical(render_cat(g), c("a" = "2/3 (66.7%)", "b" = "1/3 (33.3%)"))
  expect_identical(cat_stat(g)$a$N, 3L)

  # A zero-count level is kept (table() keeps it); render_cat shows it blank.
  h <- factor(c("a", "a", "b"), levels = c("a", "b", "c"))
  expect_identical(names(render_cat(h)), names(table(h, useNA = "no")))
  expect_identical(unname(render_cat(h)[["c"]]), "")

  # An explicit NA *level* (factor(exclude = NULL)) is not a shape users are
  # expected to create, but if it occurs render_cat still just mirrors table():
  # the NA level is counted like any category and is NOT relabelled to
  # "Missing" (which would collide with the genuine Missing row stat_tab adds).
  f <- factor(c("a", "a", "b", NA, NA), levels = c("a", "b", NA),
              exclude = NULL)
  expect_identical(names(render_cat(f)), names(table(f, useNA = "no")))
  expect_false("Missing" %in% names(render_cat(f)))
})
