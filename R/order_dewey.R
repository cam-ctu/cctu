#'sorts inputs that look like Dewey decimal numbering
#'@param x a vector of character strings
#'
#'@return  a numeric vector that sorts the input as per dewey decimal system, without confusing 10.2, vs 10.10
#'@export



order_dewey=function(x){

  if( class(x)!="character"){stop("Must input a character vector")}
#split up the string into a list of vectors broken by decimals
  X=strsplit(x,"\\.")
# FInd out the longest length
  nrow=max(sapply(X,length))
#Pad the empty elements out and return a matrix
  X=sapply(X, function(x){c(as.integer(x), rep(NA,nrow-length(x)))  })
# a trick to get X technically as a list of columns, and then allow
# order() over an arbitrary number of columns as arguments to break ties
  X=as.data.frame(t(X))
  do.call(order,X)
}


