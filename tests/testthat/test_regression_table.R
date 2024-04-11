#load(url("https://hbiostat.org/data/repo/sex.age.response.sav"))
# if you want to avoid Hmisc library
library(Hmisc)
library(rms)
getHdata(sex.age.response)



fit <- glm(response~sex, family=binomial, data=sex.age.response)
regression_table(fit, digits=3)

f <- lrm(response ~ sex + age, data=sex.age.response)
ltx <- function(fit) {
  w <- latex(fit, inline=TRUE, columns=54,
             after='', digits=3,
             before='$$X\\hat{\\beta}=$$')
  rendHTML(w, html=FALSE)
}
ltx(f)

library(rvest)
html <- read_html("https://hbiostat.org/rmsc/lrm")
table_list <- html %>% html_elements("table") %>% html_table(convert=FALSE)
# table 4 is the one to compare with
x <- table_list[[4]][2,2] %>% as.character

regression_table(fit, digits=5)

# test if have decimal point

find_dp <- function(x){
  x <- as.character(x)
  has_dp <- grepl("\\.", x)
  if(has_dp){
    dp <- nchar( strsplit(x, split="\\.")[[1]][2])
  }else{
    x <- as.integer(x)
    x <- abs(x)
    i <- 0
    k <- x %>% abs %>% log10 %>% floor
    while(0<x){
      x <- x %% 10^k
      k <- k-1
      i <- i+1
    }
    dp <- -i
  }

  return(dp)
}

find_dp_v <- Vectorize(find_dp)
delta <- find_dp_v(c(x,"1.69")) %>% diff

y <- as.numeric(c(x,"1.69"))
abs(log(y[1]/y[2]))
5*10^(-abs(delta))
#what to do if delta=0

abs( log(x,y) )  < 5*10^-delta(dp or i)
as.numeric("10.0")==as.numeric("10")
log(10/14)
log(15/20)
5*10^-2
