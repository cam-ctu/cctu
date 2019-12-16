rm(list=ls())

#library(cctu)
#library(testthat)
#library(magrittr)
#library(dplyr)
context("Test sumby")

set.seed(1012)
n <- 20
df <- data.frame(arm=rep(c("A","B"), rep(n,2)),
                 age = rnorm(2*n, mean=40,sd=10) %>% round(1),
                 gender = sample(c("Male","Female"),2*n, replace=TRUE)
                 )
summary(df)

# Check for ordered factor
df$gender <- factor(df$gender, levels = c("Male","Female"), ordered = T)

X_age <- sumby(age, arm, data=df)

mu <- with(df, mean(age[arm=="A"]))
sd <- with(df, sd(age[arm=="A"]))
answer <- paste0(format(mu, digits=3, width=2),
                " (",
                format(sd, digits=3, width=2),
                ")"
                )

test_that("continuous variable",
          expect_equivalent(answer, X_age[2,"A"])
)

test_that("categorical",{
expect_silent(X_gender <- sumby(gender , arm , data=df, total=FALSE, fig=FALSE))
r <- with(df, sum( gender[arm=="B"]=="Male"))
n <- with(df, sum( arm=="B"))
value <- X_gender[1,"B"]
expect_equivalent(r%>%as.character, gsub( "(\\d+)/.*",  "\\1", value, perl=TRUE))#".*\\((\\d+)/.*",
expect_equivalent(n%>%as.character, gsub(".*/(\\d+).*","\\1", value, perl=TRUE))# ".*/(\\d+)\\).*",
})

test_that("characterconvert",
          expect_silent(X_gender <- sumby(gender , arm , data=df, text_clean=NULL, fig=FALSE,label=NULL))

          )

test_that("sumfig direct",
          {
            fig <- sumfig(gender , arm , data=df)
            expect_is(fig, "gg")
          }
          )

test_that("interactive figure",
          {
            if(Sys.info()["sysname"] == "Windows" ){
              cmd <- paste0( R.home("bin"), '/R --ess --no-save')
            } else{
              cmd <- paste0( R.home("bin"), '/R --interactive --no-save')
            }
            fig_file=file.path(tempdir(),"fig_file.pdf")
            #deal with \ needed to be doubled, and escape sequences in gsub...
            fig_file <- gsub("\\\\","\\\\\\\\",fig_file)
            script_file=file.path(tempdir(),"interactive_sumby.R")
            script <- c(paste0("fig_file=\"",fig_file,"\""))
            writeLines(c(script, readLines("interactive_sumby.R")), script_file)
            system(cmd, input=readLines(script_file) )
            expect_equal(file.exists(fig_file), TRUE)
          }
)



test_that("empty factor",{
  expect_warning(
    X <- sumby(factor(rep(NA,40), levels=c("male","female")), arm, data=df,fig=TRUE),
    "Cannot produce a figure for a factor that is all missing"
  )
  expect_true( is.null(attr(X,"fig")))
}
)


test_that("case with binary endpoint and just want one of the rows",{
  X <- sumby(gender, arm, data=df, delete="Male")
  expect_equal( class(X),"character")
  expect_equivalent(X["Statistics"],"Female")

})
