context("test functions variable label related functions")

data(mtcars)

mtcars_labs <- within(mtcars, {
  var_lab(mpg) <- "Miles/(US) gallon"
  var_lab(cyl) <- "Number of cylinders"
  var_lab(disp) <- "Displacement (cu.in.)"
  var_lab(hp) <- "Gross horsepower"
  var_lab(drat) <- "Rear axle ratio"
  var_lab(wt) <- "Weight (lb/1000)"
  var_lab(qsec) <- "1/4 mile time"
  var_lab(vs) <- "V/S"
  val_lab(vs) <- c("V-shaped" = 0, "straight" = 1)
  var_lab(am) <- "Transmission"
  val_lab(am) <- c(automatic = 0, manual = 1)
  var_lab(gear) <- "Number of forward gears"
  var_lab(carb) <- "Number of carburetors"
})

test_that("Check assign variable lables", {
  expect_identical(var_lab(mtcars$am), NULL)
  empty_list <- vector(mode = "list", length = ncol(mtcars))
  names(empty_list) <- names(mtcars)
  expect_identical(var_lab(mtcars), empty_list)


  var_lab(mtcars$am) <- "Transmission"
  empty_list$am <- "Transmission"

  expect_identical(var_lab(mtcars$am), "Transmission")
  expect_identical(var_lab(mtcars), empty_list)

  expect_error(var_lab(mtcars$am) <- 1)
  expect_error(var_lab(mtcars$am) <- c("One", "Two"))
  expect_error(var_lab(mtcars) <- "Mydata")

  mtcars$am_tmp <- mtcars$am
  var_lab(mtcars$am_tmp) <- NULL

  expect_identical(mtcars$am_tmp, drop_lab(mtcars$am))

  expect_false(has.label(mtcars$am_tmp))

  expect_null(val_lab(mtcars))
})

test_that("Drop variable lables", {
  var_lab(mtcars$am) <- "Transmission"

  empty_list <- vector(mode = "list", length = ncol(mtcars))
  names(empty_list) <- names(mtcars)

  expect_identical(var_lab(drop_lab(mtcars$am)), NULL)
  expect_identical(var_lab(drop_lab(mtcars_labs)), empty_list)
})

context("test functions variable value label related functions")

test_that("Assign value lables", {
  a <- 1
  expect_error({
    val_lab(a) <- c(a = 1, b = 1)
  })
  expect_warning({
    val_lab(a) <- c(1, b = 2)
  })
  val_lab(a) <- c(a = 1, a = 2)

  dd <- data.frame(a = 1:3, b = 3:1, d = 3)

  val_lab(dd$a) <- c(a = 1)
  val_lab(dd$b) <- c(b = 2)
  val_lab(dd$d) <- c(d = 3)

  expect_identical(list(
    "a" = c(a = 1),
    "b" = c(b = 2),
    "d" = c(d = 3)
  ), val_lab(dd))

  expect_true(has.labels(a))
  a <- unval(a)
  expect_false(has.labels(a))
  expect_identical(unval(a), 1)

  dd <- data.frame(a = 1:3, b = 3:1, d = 3)
  dd$a <- as.factor(dd$a)
  val_lab(dd$a) <- c(a = 1, b = 2)
})


context("test functions drop labels")

test_that("Label/labls attributes", {
  data(mtcars)

  expect_identical(unlab(mtcars_labs), mtcars)

  var_with_lab <- rep(1:2, 5)
  var_lab(var_with_lab) <- "Income"
  val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)

  var_nolab <- rep(1:2, 5)
  var_ut <- copy_lab(var_nolab, var_with_lab)
  expect_identical(var_ut, var_with_lab)

  var_nolab <- as.character(rep(1:2, 5))
  expect_error(copy_lab(var_nolab, var_with_lab))

  expect_null(unlab(NULL))
})


test_that("Convert value label to value", {
  vec <- 1:7

  expect_identical(lab2val(vec), vec)

  mat <- matrix(1:9, ncol = 3)

  expect_identical(lab2val(mat), mat)

  out_mat <- mat
  expect_identical(lab2val(mat), out_mat)

  mat[, 3] <- NA
  out_mat[, 3] <- NA
  expect_identical(lab2val(mat), out_mat)
  expect_identical(lab2val(numeric(0)), numeric(0))

  df <- data.frame(a = 1:3, b = 3:1, d = letters[1:3], e = NA, stringsAsFactors = FALSE)

  val_lab(df$a) <- c(a = 1, b = 2)
  val_lab(df$b) <- c(a = 45)
  val_lab(df$d) <- c(a = "b", b = "d", e = "c")
  val_lab(df$e) <- c(a = 1, b = 2)

  var_lab(df$b) <- "Column b"

  out_df <- unval(df)

  out_df$a[1] <- "a"
  out_df$a[2] <- "b"
  out_df$b <- as.character(out_df$b)
  var_lab(out_df$b) <- "Column b"
  out_df$d[2] <- "a"
  out_df$d[3] <- "e"
  out_df$e <- as.character(out_df$e)

  expect_identical(unlab(lab2val(df)), unlab(out_df))

  aaa <- c(1:3, 3.5)
  val_lab(aaa) <- c(a = 1, b = 2)

  expect_identical(class(lab2val(aaa)), "character")

  var_lab(aaa) <- "Test"
  aaa <- to_factor(aaa)
  val_lab(aaa) <- c(a = 1, b = 2)
  expect_identical(var_lab(aaa), "Test")

  expect_error(
    val_lab(aaa) <- c(1, 2),
    "'val_lab' - labels should be named vector."
  )
})
