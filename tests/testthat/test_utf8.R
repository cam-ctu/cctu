context("utf8")


rm(list=ls())

library(cctu)


test_that(
 "check inputs for detection", {
  expect_equal(dim(detect_invalid_utf8(ae_df[rep(69:71,3),rep(2:4,2)])),c(6,3))
  expect_equal( dim(detect_invalid_utf8(ae_df)),c(1,3))
  expect_equal( dim(detect_invalid_utf8(ae_df[,-3])),c(0,3))
  expect_error( detect_invalid_utf8(1:10), "input a data.frame")
}
)


test_that("check removal",{


  ae2 <- remove_invalid_utf8(ae_df)
  expect_s3_class(ae2, "data.frame")
  expect_equal(nrow(detect_invalid_utf8(ae2)),0)
  expect_error( remove_invalid_utf8(1:4), "input a data.frame")

  })

test_that("write_table warning",{

  .old_meta <- get_meta_table()
  set_meta_table(cctu::meta_table_example)
  expect_warning(write_table(ae_df,number="1.10"),
                 "Invalid non-UTF8 characters found")
  set_meta_table(.old_meta)
  rm(.old_meta, .parent)
})

