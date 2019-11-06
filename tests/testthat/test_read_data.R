
context("read_data")

 data_table <- data.frame(name=c("dirtydata","meta"),
                          file=c(system.file("extdata","dirtydata.csv", package="cctu"),
                                 system.file("extdata","meta_table.xlsx", package="cctu")),
                          folder="")




 test_that("basic case",{expect_is(data_table_summary(data_table), "data.frame")
 options("verbose"=TRUE)

 expect_false(exists("dirtydata"))
 expect_false(exists("meta"))
 read_data(data_table)
 expect_true(exists("dirtydata"))
 expect_true(exists("meta"))
 rm(dirtydata, meta)
 data2 <- data_table
 names(data2) <- c("link","path","dolf")
 read_data(data2, name_variable="link", file_variable="path")
 expect_true(exists("dirtydata"))
 expect_true(exists("meta"))
 rm(dirtydata, meta)

 }
)

 test_that("non standard case",{
 X <- read_data(data_table[1,], frame=NULL, stringsAsFactors=FALSE)
 expect_false(is.factor(X$gender))
 expect_error(read_data("dirtydata",data_table, fun=readxl::read_excel, frame=NULL))
 expect_warning(read_data("dirtydata",data_table, fun=print, frame=NULL),
                "this function is designed to be used for reading in data. You are calling: print"
 )


 old_fn <- base::requireNamespace
 myrequireNamespace <- function(...) FALSE
 unlockBinding("requireNamespace", as.environment("package:base"))
 assign("requireNamespace",myrequireNamespace, "package:base")
 expect_error(read_data("meta",data_table,  frame=NULL),"Package \"readxl\" needed for this function to load excel files")
 assign("requireNamespace",old_fn, "package:base")
 lockBinding("requireNamespace", as.environment("package:base"))
 rm(old_fn, myrequireNamespace)

}
)






