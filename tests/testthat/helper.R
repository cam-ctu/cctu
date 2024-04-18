
# This works out how many sig figures (for integers), or how many dp
# a character representation of a number is written to.

find_dp <- function(x){
  x <- as.character(x)
  has_dp <- grepl("\\.", x)
  # for dcimal numbers
  dp_positive <- nchar( strsplit(x, split="\\.")[[1]][2])
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
  x <- as.numeric(x)
  y <- as.numeric(y)
  sig_fig_test <- signif(x,-k+1)==signif(y,-k+1)
  dp <- pmin(dp_x,dp_y)
  round_test <- round(x,dp)==round(y,dp)
  ifelse( dp <=0, sig_fig_test, round_test)
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
