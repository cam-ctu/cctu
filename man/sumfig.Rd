% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumfig.R
\name{sumfig}
\alias{sumfig}
\title{Produce ggplot to represent summary statistics graphically, either boxplot
or barchart}
\usage{
sumfig(variable, arm, label = NULL, data = parent.frame(), ...)
}
\arguments{
\item{variable}{A variable of interest to be summarised. Works for several
classes: numeric, integer, factor, character.}

\item{arm}{A variable to break down the summary by, typically an arm in a
RCT. Or a constant variable for 1-armed study}

\item{label}{a text string to label the variable by, used to add in units
or full words with spaces rather than acronyms. Defaults to NULL.}

\item{data}{optional a data frame, or envirnoment, in which to find the
variable and arm values. Defaults to parent.frame()}

\item{...}{list of other values to input into the aes() function}
}
\value{
a ggplot object, either a boxplot or barchart
}
\description{
Produce ggplot to represent summary statistics graphically, either boxplot
or barchart
}
