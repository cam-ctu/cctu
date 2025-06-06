% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_manual.R
\name{write_manual}
\alias{write_manual}
\title{Function to write to meta-table and clean up without creating a table,text,
or figure. For manual use}
\usage{
write_manual(
  number = cctu_env$number,
  clean_up = TRUE,
  verbose = options()$verbose
)
}
\arguments{
\item{number}{the number used to as a suffix in the output filename, and to
link to TableofTables. Default is to use the value in the cctu_env package
 environment that is set within \code{\link{attach_pop}}.}

\item{clean_up}{logical to invoke the \code{\link{clean_up}} function at the
end. Defaults to TRUE}

\item{verbose}{logical to print information on changes to the global
environment or external files. Defaults to options()$verbose.}
}
\value{
Edits the meta_table object with the calling program and optionally
cleans up. No return object.  Meant for use if a table or figure for a
report has been created manually outside of \code{\link{write_ggplot}} or
 \code{\link{write_table}}
}
\description{
Function to write to meta-table and clean up without creating a table,text,
or figure. For manual use
}
\seealso{
\code{\link{write_ggplot}}  \code{\link{write_table}}
}
