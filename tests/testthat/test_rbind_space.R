
context("rbind_space")
library(cctu)

test_that("try different classes of object",{
x <- matrix(1:4, ncol=2)
expect_is(rbind_space(x,x), "matrix")
x <- data.frame(x=c("a","b"), y=3:4,stringsAsFactors = FALSE)
expect_is(rbind_space(x,x), "data.frame")
x <- data.frame(x=c("a","b"), y=3:4)
expect_failure(expect_warning(rbind_space(x,x)))
x <- factor(c("a","b"))
expect_is(rbind_space(x,x), "matrix")
x <- c(pi,exp(1))
expect_is(rbind_space(x,x), "matrix")

x <- 1:2
x <- xtabs(~x+x)
expect_error(rbind_space(x,x))
}
)