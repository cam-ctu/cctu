% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_stat.R
\name{render_cat}
\alias{render_cat}
\title{Render categorical values for table output.}
\usage{
render_cat(x, ...)
}
\arguments{
\item{x}{A vector of type \code{factor}, \code{character} or \code{logical}.}

\item{...}{Further arguments, passed to \code{\link{cat_stat}}.}
}
\value{
A \code{character} vector. Each element is to be displayed in a
separate cell in the table. The \code{\link{names}} of the vector are the
labels to use in the table.
}
\description{
Called from \code{\link{cttab}} by default to render categorical (i.e.
\code{factor}, \code{character} or \code{logical}) values for displaying
in the table.
}
\details{
This function was used by \code{link{cttab}} to render categorical variables.
It essentially uses the values returned by \code{\link{cat_stat}} and
put values to a vector. You can modified this to show any values you
want, checkout the example below.
}
\examples{
y <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
y[1:10] <- NA
render_cat(y)
}
\seealso{
\code{\link{signif_pad}}
\code{\link{round_pad}}
\code{\link{num_stat}}
}
\keyword{utilities}
