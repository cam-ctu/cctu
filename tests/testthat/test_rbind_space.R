
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

test_that("wrong number of columns",{
          a <- matrix(1, nrow=2, ncol=3)
          b <- matrix(1, nrow=2, ncol=2)
          expect_error(rbind_space(a,b), "the number of columns do not match")
}
          )

test_that("mixture of matrix and 1-d character",
          {
            a <- matrix(1, nrow=2, ncol=3)
            b <- c("A","B","C")
            c <- 1:3
            d <- rnorm(3)
            attr(d,"other") <- "not now a vector"
            expect_is(Reduce(rbind_space, list(a,b,c,d)), "matrix")
          }

          )


test_that("colnames edits",{

  X <- data.frame("Param"=1:2, "Estmate (S.E.)"=c("1 (2)","3 (4)"), check.names = FALSE)
  Y <- rbind_space(X,X)
  expect_equal(colnames(Y), colnames(X))
})

