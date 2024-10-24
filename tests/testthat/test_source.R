test_that("using local and options in the cctu_source",{

# vanilla in global env
# increment.R is file with the one line :  x <- x+1

assign("x",1, envir=globalenv())
source(test_path("increment.R"))
expect_equal(x,2)


# if we drop the parent_frame line in cctu::source then this also fails
# can't udnerstand what it does
source(test_path("increment.R"), local=TRUE)
expect_equal(x,3)


# FAILS on auto testing-  not sure why
# inside a function env, but using global
#  changing gloablenv or test_env has no effect.
assign("x",3, envir=globalenv())
y <- run(1)
expect_equal(x,4)
expect_equal(y,1)


# inside a function env, but using local
assign("x",3, envir=globalenv())
y <- run(1, local=TRUE)
expect_equal(x,3)
expect_equal(y,2)



# using options
options(cctu_source_local=FALSE)
assign("x",3, envir=globalenv())
y <- run(1)
expect_equal(x,4)
expect_equal(y,1)

options(cctu_source_local=TRUE)
assign("x",3, envir=globalenv())
y <- run(1)
expect_equal(x,3)
expect_equal(y,2)

options(cctu_source_local=NULL)

})
