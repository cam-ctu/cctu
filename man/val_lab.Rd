% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{val_lab}
\alias{val_lab}
\alias{val_lab<-}
\alias{has.labels}
\alias{unval}
\title{Set or get value labels}
\usage{
val_lab(x)

val_lab(x) <- value

has.labels(x)

unval(x)
}
\arguments{
\item{x}{Variable(s). Vector/data.frame/list.}

\item{value}{Named vector. Names of vector are labels for the appropriate
values of variable x. Names can be duplicated, but not the value.}
}
\value{
\code{val_lab} return value labels (named vector). If labels doesn't
  exist it return NULL . \code{val_lab<-} return variable (vector x) which
  contains value labels.
}
\description{
These functions set/get/drop value labels. Duplicated values
are not allowed. If argument \code{x} is data.frame or list then labels
applied to all
elements of data.frame/list. To drop value labels, use
\code{val_lab(var) <- NULL} or \code{unval(var)}. For variable labels see
 \link{var_lab}.
\describe{
\item{\code{val_lab}}{ returns value labels or NULL if labels doesn't
exist.}
\item{\code{val_lab<-}}{ set value labels.}
\item{\code{unval}}{ drops value labels.}
\item{\code{has.labels}}{ check if value labels exists.}
}
}
\details{
Value labels are stored in attribute "labels"
  (\code{attr(x,"labels")}).
}
\examples{
# toy example
set.seed(123)
# score - evaluation of tested product

score <- sample(-1:1, 20, replace = TRUE)
var_lab(score) <- "Evaluation of tested brand"
val_lab(score) <- c(
  "Dislike it" = -1,
  "So-so" = 0,
  "Like it" = 1
)

}
\references{
This is a modified version from `expss` package.
}
