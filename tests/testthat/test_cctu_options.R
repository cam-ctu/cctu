# Reset cctu_options + the cctu_* base options touched by each test, so test
# order can't leak state.
local_cctu_options <- function(.envir = parent.frame()) {
  prev_env <- cctu_env$options
  withr::defer(cctu_env$options <- prev_env, envir = .envir)

  prev_base <- options()[grep("^cctu_", names(options()))]
  withr::defer(
    {
      curr <- options()[grep("^cctu_", names(options()))]
      to_clear <- setdiff(names(curr), names(prev_base))
      if (length(to_clear)) {
        do.call(options, setNames(rep(list(NULL), length(to_clear)), to_clear))
      }
      if (length(prev_base)) options(prev_base)
    },
    envir = .envir
  )
}


test_that("cctu_options() with no args returns the full effective set", {
  local_cctu_options()
  cctu_options(reset = TRUE)
  out <- cctu_options()
  expect_type(out, "list")
  expect_named(out, names(cctu:::.cctu_default_opts))
  expect_identical(out$digits, 3)
  expect_identical(out$output, "Output")
  expect_identical(out$render_num, "Median [Min, Max]")
})


test_that("cctu_options(name = value) sets and returns previous values", {
  local_cctu_options()
  cctu_options(reset = TRUE)

  prev <- cctu_options(digits = 5, p_digits = 2)
  expect_identical(prev$digits, 3)
  expect_identical(prev$p_digits, 4)
  expect_identical(cctu_opt("digits"), 5)
  expect_identical(cctu_opt("p_digits"), 2)
})


test_that("base options() take precedence over cctu_options()", {
  local_cctu_options()
  cctu_options(reset = TRUE)

  cctu_options(digits = 5)
  expect_identical(cctu_opt("digits"), 5)

  options(cctu_digits = 9)
  expect_identical(cctu_opt("digits"), 9)

  # And once the base option is cleared, the cctu_options() value re-emerges.
  options(cctu_digits = NULL)
  expect_identical(cctu_opt("digits"), 5)
})


test_that("cctu_opt() falls back to packaged defaults when nothing is set", {
  local_cctu_options()
  cctu_options(reset = TRUE)
  expect_identical(cctu_opt("digits"), 3)
  expect_identical(cctu_opt("subjid_string"), "subjid")
  expect_identical(cctu_opt("fig_format"), c("png", "eps"))
})


test_that("cctu_options(reset = TRUE) restores packaged defaults", {
  local_cctu_options()
  cctu_options(reset = TRUE)

  cctu_options(digits = 9, blinded = TRUE)
  cctu_options(reset = TRUE)
  expect_identical(cctu_opt("digits"), 3)
  expect_identical(cctu_opt("blinded"), FALSE)
})


test_that("cctu_options(reset = TRUE, ...) resets first then applies", {
  local_cctu_options()
  cctu_options(digits = 9)
  res <- cctu_options(reset = TRUE, digits = 7)
  # `prev` reflects state *after* the reset, before the set.
  expect_identical(res$digits, 3)
  expect_identical(cctu_opt("digits"), 7)
})


test_that("cctu_options() warns when an option is shadowed by base options", {
  local_cctu_options()
  cctu_options(reset = TRUE)
  options(cctu_digits = 11)
  expect_warning(
    cctu_options(digits = 4),
    "takes precedence"
  )
  # The cctu_env value was still updated, but cctu_opt() returns the base value.
  expect_identical(cctu_env$options$digits, 4)
  expect_identical(cctu_opt("digits"), 11)
})


test_that("cctu_options() rejects unnamed and unknown arguments", {
  local_cctu_options()
  expect_error(cctu_options(3), "must be named")
  expect_error(cctu_options(foo = 1), "Unknown cctu option")
  expect_error(cctu_options(digits = 4, foo = 1, bar = 2), "Unknown cctu options")
})


test_that("cctu_opt() rejects unknown names", {
  expect_error(cctu_opt("foo"), "not a known cctu option")
})
