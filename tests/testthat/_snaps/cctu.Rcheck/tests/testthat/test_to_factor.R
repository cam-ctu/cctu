context("Vector type conversion")

df <- data.frame(
  a = 1:3, b = 3:1, d = letters[1:3], e = c(0, 1, 1),
  stringsAsFactors = FALSE
)

var_lab(df$a) <- "Var A"
var_lab(df$b) <- "Var B"
var_lab(df$d) <- "Var D"
var_lab(df$e) <- "Var E"

test_that("Test to_factor", {
  expect_identical(levels(to_factor(df$a)), c("1", "2", "3"))

  val_lab(df$a) <- c(a = 1, b = 2)
  expect_true(is.ordered(to_factor(df$a)))
  expect_false(is.ordered(to_factor(df$a, ordered = F)))
  expect_identical(levels(to_factor(df$a)), c("a", "b"))
  expect_identical(var_lab(to_factor(df$a)), var_lab(df$a))

  val_lab(df$d) <- c(a = "a", a = "b", e = "c")
  expect_identical(levels(to_factor(df$d)), c("a", "e"))

  expect_identical(
    {
      df$a[df$a == 1] <- NA
      levels(to_factor(df$a, drop.levels = T))
    },
    c("b")
  )

  df$a[is.na(df$a)] <- 1

  expect_identical(
    {
      df$a[df$a == 1] <- ""
      levels(to_factor(df$a, drop.levels = T))
    },
    c("b")
  )

  expect_identical(unlab(to_numeric(df$a)), c(NA, 2, 3))
  expect_identical(var_lab(to_numeric(df$a)), var_lab(df$a))


  expect_identical(unlab(to_character(df$b)), c("3", "2", "1"))
  expect_identical(var_lab(to_character(df$b)), var_lab(df$b))
  val_lab(df$d) <- c(a = "a", a = "b", e = "c")
  expect_identical(unlab(to_character(df$d)), c("a", "b", "c"))

  expect_identical(unlab(lab2val(df$d)), c("a", "a", "e"))
  expect_identical(var_lab(lab2val(df$d)), "Var D")

  expect_identical(unlab(to_logical(df$e)), c(F, T, T))
  expect_identical(var_lab(to_logical(df$e)), "Var E")
})
