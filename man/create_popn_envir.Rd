% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_popn_envir.R
\name{create_popn_envir}
\alias{create_popn_envir}
\title{create environments contain a set of data frames filtered for each population}
\usage{
create_popn_envir(
  data_names,
  popn,
  subjid_string = "subjid",
  rm_from_frame = TRUE,
  frame = parent.frame(),
  verbose = options()$verbose
)
}
\arguments{
\item{data_names}{a character vector of the key data frames to be filtered}

\item{popn}{a data frame one-row per participant  a subjid variable and a
column for each population indicating if a participant belongs to the
population. The names of the variables must match up to the values in
meta_table$Population.}

\item{subjid_string}{the character naming the column used in each data frame
from data_names and popn, to identify the subjid.}

\item{rm_from_frame}{a logical indicating if the data sets should be removed
from the frame environment}

\item{frame}{the frame in which the original list of data sets are found,
and  in which the new environments will be contained.}

\item{verbose}{logical to print information on changes to the global
environment or external files. Defaults to options()$verbose.}
}
\description{
create environments contain a set of data frames filtered for each population
}
\details{
Now to use the data frames defined above in data_names you have to
 either directly call attach(safety), for example or look-up with
 attach_pop(1.01) for a table number. There are separate R environments made
  for each of the populations in population & population_title be careful to
   use detach(safety) - this is automated with write_table() and
   write_ggplot()
}
