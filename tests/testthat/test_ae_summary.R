# Small synthetic AE-style data shared across tests.
ae_demo_data <- function() {
  adsl <- data.frame(
    id  = sprintf("S%02d", 1:10),
    arm = rep(c("A", "B"), each = 5),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    id  = c("S01", "S01", "S02", "S03", "S06", "S06", "S07"),
    soc = c("Skin", "GI", "Skin", "Skin", "GI", "Skin", "Skin"),
    pt  = c("Rash", "Nausea", "Rash", "Pruritus", "Nausea", "Rash", "Rash"),
    gr  = c("Grade 1", "Grade 3", "Grade 2", "Grade 1",
            "Grade 2", "Grade 3", "Grade 1"),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, ae = ae)
}

test_that("ae_summary returns a rendering-ready cttab with a label column", {
  d <- ae_demo_data()
  out <- ae_summary(d$ae, d$adsl, id_var = "id", class_var = "soc",
                    term_var = "pt", trt_var = "arm", grade_var = "gr")

  # Styled-by-default: cttab class, label first, row_style stamped.
  expect_s3_class(out, "cttab")
  expect_equal(names(out)[1], "label")
  expect_length(attr(out, "row_style"), nrow(out))

  # Helper / hierarchy columns are folded into label and dropped, leaving
  # only label + the data (arm) columns for the renderer.
  expect_false(any(c("level", "soc", "pt") %in% names(out)))
  expect_true(all(c("A", "B", "Total") %in% names(out)))
})

test_that("ae_summary bolds any-event, class and nested term rows", {
  d <- ae_demo_data()
  # Vertical grade layout (default): grade sub-rows nest beneath each term.
  out <- ae_summary(d$ae, d$adsl, id_var = "id", class_var = "soc",
                    term_var = "pt", trt_var = "arm", grade_var = "gr")
  rs  <- attr(out, "row_style")
  lab <- trimws(out$label)

  is_grade <- grepl("^Grade", lab)
  is_any   <- lab == "Participants with any event"
  is_class <- lab %in% c("Skin", "GI")
  is_term  <- !(is_grade | is_any | is_class)
  bold     <- grepl("bold", rs)
  indent   <- grepl("indent", rs)

  # Any-event + class rows: bold, not indented.
  expect_true(all(bold[is_any | is_class]))
  expect_false(any(indent[is_any | is_class]))
  # Term rows carry nested grade sub-rows, so they are bold *and* indented.
  expect_true(any(is_term))
  expect_true(all(bold[is_term]))
  expect_true(all(indent[is_term]))
  # Grade sub-rows: indented, not bold.
  expect_true(all(indent[is_grade]))
  expect_false(any(bold[is_grade]))
})

test_that("ae_summary leaves term rows unbolded when they have no sub-rows", {
  d <- ae_demo_data()
  # Horizontal layout puts grades in columns, so term rows are leaf rows.
  out <- ae_summary(d$ae, d$adsl, id_var = "id", class_var = "soc",
                    term_var = "pt", trt_var = "arm", grade_var = "gr",
                    grade_layout = "horizontal")
  rs  <- attr(out, "row_style")
  lab <- trimws(out$label)
  is_term <- !(lab == "Participants with any event" | lab %in% c("Skin", "GI"))

  expect_true(any(is_term))
  expect_false(any(grepl("bold", rs[is_term])))   # leaf terms are not bold
  expect_true(all(grepl("indent", rs[is_term])))  # but still indented
})

test_that("ae_summary indents nested labels by depth", {
  d <- ae_demo_data()
  out <- ae_summary(d$ae, d$adsl, id_var = "id", class_var = "soc",
                    term_var = "pt", trt_var = "arm", grade_var = "gr")
  lab <- out$label

  # Class rows (bold tops) carry no leading indent; term rows are indented
  # one level (3 spaces) and grade rows two levels (6 spaces).
  leading <- function(x) attr(regexpr("^ *", x), "match.length")
  any_row   <- which(trimws(lab) == "Participants with any event")
  class_row <- which(trimws(lab) == "Skin")
  term_row  <- which(trimws(lab) == "Rash")[1]
  grade_row <- which(trimws(lab) == "Grade 1")[1]

  expect_equal(leading(lab[any_row]),   0L)
  expect_equal(leading(lab[class_row]), 0L)
  expect_equal(leading(lab[term_row]),  3L)
  expect_equal(leading(lab[grade_row]), 6L)
})

test_that("ae_summary counting is preserved (any-event denominators)", {
  d <- ae_demo_data()
  out <- ae_summary(d$ae, d$adsl, id_var = "id", class_var = "soc",
                    term_var = "pt", trt_var = "arm", grade_var = "gr")

  any_row <- which(trimws(out$label) == "Participants with any event")
  # 5 of 10 subjects (3/5 in A, 2/5 in B) have at least one event.
  expect_equal(out$A[any_row],     "3 (60.0)")
  expect_equal(out$B[any_row],     "2 (40.0)")
  expect_equal(out$Total[any_row], "5 (50.0)")
})

test_that("ae_summary flat (no class) bolds only the any-event row", {
  d <- ae_demo_data()
  out <- ae_summary(d$ae, d$adsl, id_var = "id", term_var = "pt",
                    trt_var = "arm")
  rs <- attr(out, "row_style")

  expect_s3_class(out, "cttab")
  expect_false("soc" %in% names(out))
  # Only the single any-event row is bold; all terms are indented.
  expect_equal(sum(grepl("bold", rs)), 1L)
  expect_match(rs[1], "bold")
  expect_equal(sum(grepl("indent", rs)), nrow(out) - 1L)
})
