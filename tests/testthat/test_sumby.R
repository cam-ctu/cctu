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



