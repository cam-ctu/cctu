
context("test clean_names")

test_that("check output, verbose, and convert",
          {
df <-  data.frame(x=1)
names(df) <- "_ A hyphen-/slash . bad__name_  "
df2 <- clean_names(df, convert=FALSE)
output <- capture_output(clean_names(df, verbose=TRUE))
expect_equal(output, "df given clean names")
expect_equal(names(df),"a_hyphen_slash_bad_name")
expect_equal(df2,"a_hyphen_slash_bad_name")
})
