% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{copy_lab}
\alias{copy_lab}
\title{Copy variable label and value labels}
\usage{
copy_lab(new_var, old_var, strict = TRUE)
}
\arguments{
\item{new_var}{Variable to be copied to.}

\item{old_var}{Variable to be copied from.}

\item{strict}{Should the variables should be the same \code{mode}.}
}
\value{
\code{copy_lab} returns \code{new_var} with same variable label
and value labels as \code{old_var}.
}
\description{
\code{copy_lab} copy the variable label and value labels or returns
\code{old_var} to
variable \code{new_var} and return \code{new_var}.
}
\examples{
var_with_lab <- rep(1:2, 5)
var_lab(var_with_lab) <- "Income"
val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)
var_nolab <- rep(1:2, 10)
var_ut <- copy_lab(var_nolab, var_with_lab)
}
