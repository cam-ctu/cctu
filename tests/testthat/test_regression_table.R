#load(url("https://hbiostat.org/data/repo/sex.age.response.sav"))
# if you want to avoid Hmisc library

fit <- glm(response~sex, family=binomial, data=sex.age.response)
regression_table(fit, digits=4)

library(Hmisc)
getHdata(sex.age.response)
f <- lrm(response ~ sex + age, data=d)
ltx <- function(fit) {
  w <- latex(fit, inline=TRUE, columns=54,
             after='', digits=3,
             before='$$X\\hat{\\beta}=$$')
  rendHTML(w, html=FALSE)
}
ltx(f)
