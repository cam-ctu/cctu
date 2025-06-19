test_that("using local and options in the cctu_source", {
  # vanilla in global env
  # increment.R is file with the one line :  x <- x+1
  # test_that() creates its own evaluation environment so we need to use that

  print("test1")
  x <- 1
  envir <- environment()
  source(test_path("increment.R"), local = envir)
  expect_equal(x, 2)


  #
  print("test2")
  # if we drop the parent_frame line in cctu::source then this also fails
  # can't udnerstand what it does
  source(test_path("increment.R"), local = TRUE)
  expect_equal(x, 3)

  print("test3")
  # FAILS on auto testing-  not sure why
  # inside a function env, but using global
  #  changing gloablenv or test_env has no effect.

  print("test4")
  # withr::local_options(testthat_topenv = test_env())


  # opts <- options(topLevelEnvironment=topenv(environment(cttab)))

  run <- function(x, ...) {
    print(environment())
    source(test_path("increment.R"), ...)
    x
  }

  print("test4")


  x <- 3
  y <- run(1, local = envir)
  expect_equal(x, 4)
  expect_equal(y, 1)

  print("test5")

  # inside a function env, but using local
  x <- 3
  y <- run(1, local = TRUE)
  expect_equal(x, 3)
  expect_equal(y, 2)


  print("test6")
  # using options
  opts <- options(cctu_source_local = FALSE)
  on.exit(options(opts), add = TRUE, after = FALSE)
  assign("x", 3, envir = .GlobalEnv)
  on.exit(rm(x, pos = 1), add = TRUE, after = FALSE)
  y <- run(1)
  expect_equal(.GlobalEnv$x, 4)
  expect_equal(y, 1)

  print("test7")
  options(cctu_source_local = TRUE)
  x <- 3
  y <- run(1)
  expect_equal(x, 3)
  expect_equal(y, 2)
})
