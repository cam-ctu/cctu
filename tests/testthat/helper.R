###  for testing the source()

run <- function(x, ...){
  print(environment())
  print(search())
  source(test_path("increment.R"),...)
  x
}




# This works out how many sig figures (for integers), or how many dp
# a character representation of a number is written to.

find_dp <- function(x){
  x <- as.character(x)
  has_dp <- grepl("\\.", x)
  # for dcimal numbers
  dp_positive <- sapply( strsplit(x, split="\\."), function(x){nchar(x[2])})
  # for integers - this is not vectorised at all thoguh...
  x <- as.integer(x)
  x <- ifelse(is.na(x),0,x)
  x <- abs(x)
  dp_negative <- x
  for( row in 1:length(x)){
    z <- x[row]
    i <- -1
    k <- z %>% abs %>% log10 %>% floor
    while(0<z){
      z <- z %% 10^k
      k <- k-1
      i <- i+1
    }
    dp_negative[row] <- -i
  }
  ifelse(has_dp, dp_positive, dp_negative)
  # dp_positive is dp so comare with round(),
  # bu dp_negative is number of sig figs (for integers), so use signif()
}

# This compares two numbers possibly with different rounding applied
# to check if they are consistent.
compare_with_rounding <- function(x,y, p_less_than=TRUE){
  #for p-values
  if( p_less_than){
    y <- gsub("<","",y)
    x <- gsub("<","",x)
  }
  dp_x <- find_dp(x)
  dp_y <- find_dp(y)
  k <- pmax(dp_x, dp_y)
  dp <- pmin(dp_x,dp_y)
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  sig_fig_test <- round_test <- rep(0,n)
  for( i in 1:n){
    # the digits argument is not vectorised.
    sig_fig_test[i] <- signif(x[i],-k[i]+1)==signif(y[i],-k[i]+1)
    round_test[i] <- round(x[i],dp[i])==round(y[i],dp[i])
  }
  ans <- ifelse( dp <=0, sig_fig_test, round_test)
  #cbind(x,y,dp_x,dp_y, dp, ans)
  as.logical(ans)
}


### document how the data was obtained from websites
#  done on 18 April 2024


# load(url("https://hbiostat.org/data/repo/sex.age.response.sav"))
# library(rvest)
# html <- read_html("https://hbiostat.org/rmsc/lrm")
# table_list <- html %>% html_elements("table") %>% html_table(convert=FALSE)
# # table 4 is the one to compare with
# x_ref <- table_list[[4]] %>% as.data.frame
# save(sex.age.response, x_ref, file= test_path("fixtures","logistic.Rdata"))


# load(url("https://hbiostat.org/data/repo/bodyfat.rda"))
#
# html <- read_html("https://hbiostat.org/rmsc/bodyfat#learn-predictor-transformations-and-interactions-from-simple-model")
# table_list <- html %>% html_elements("table") %>% html_table(convert=FALSE)
# x_ref <- table_list[[3]] %>% as.data.frame
#
# save( bodyfat, x_ref, file=test_path("fixtures", "linear.Rdata"))



# load(url("https://hbiostat.org/data/repo/cdystonia.sav"))
# data.table::setDT(cdystonia)
# cdystonia[, uid := paste(site, id)]
# baseline <- cdystonia[week == 0]
# baseline[, week := NULL]
# data.table::setnames(baseline, 'twstrs', 'twstrs0')
# followup <- cdystonia[week > 0, .(uid, week, twstrs)]
# data.table::setkey(baseline, uid)
# data.table::setkey(followup, uid, week)
# both     <- Hmisc:::Merge(baseline, followup, id = ~ uid)
# both     <- both[! is.na(week)]
#
# html <- rvest::read_html("https://hbiostat.org/rmsc/long#using-generalized-least-squares")
# table_list <- html %>% rvest::html_elements("table") %>% rvest::html_table(convert=FALSE)
# x_ref <- table_list[[4]] %>% as.data.frame
#
#
# save(both, x_ref, file=test_path("fixtures", "gls.Rdata") )


## Coxph  taking the table on page 44 ( printed as PAge 43) of the vignette
## "THe Survival Pacakge"

# download.file("https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf",
#               test_path("fixtures", "surv.pdf"),
#               mode = "wb"
#               )
# library(pdftools)
# txt <- pdf_text(test_path("fixtures", "surv.pdf"))
# page44 <- strsplit(txt[44],split="\n")[[1]]
# X <- sapply( page44[39:41], strsplit,  split="\\s+", simplify="TRUE", USE.NAMES=FALSE)
# X <- Reduce(rbind, X)
# varnames <- strsplit(page44[[38]], split="\\s+")[[1]]
# X <- X[,-1]
# varnames[1] <- "covariate"
# colnames(X) <- varnames
# save(X, file = test_path("fixtures", "survival.Rdata"))
#

# LMe  use the paper https://www.jstatsoft.org/article/view/v067i01
# doi 10.18637/jss.v067.i01
# download.file("https://www.jstatsoft.org/index.php/jss/article/view/v067i01/946",
#               test_path("fixtures", "v67i01.pdf"),
#               mode = "wb"
#               )
#
# # library(pdftools)
# txt <- pdf_text(test_path("fixtures", "v67i01.pdf"))
# page32 <- strsplit(txt[32],split="\n")[[1]]
# X <- sapply( page32[13:14], strsplit,  split="\\s+", simplify="TRUE", USE.NAMES=FALSE)
# X <- Reduce(rbind, X)
# colnames(X) <- c("covariate","est", "se","t")
# page31 <- strsplit(txt[31],split="\n")[[1]]
# Y <- sapply( page31[50:53], strsplit,  split="\\s+", simplify="TRUE", USE.NAMES=FALSE)
# Y <- Reduce(rbind, Y)
# varnames <- strsplit(page31[[49]], split="\\s+")[[1]]
# colnames(Y) <- varnames
# save(X,Y, file = test_path("fixtures", "lme.Rdata"))
#
