% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_word_xml.R
\name{create_word_xml}
\alias{create_word_xml}
\title{Produces the final xml file}
\usage{
create_word_xml(report_title, author, datestamp = format(Sys.time(), format =
  "\%H:\%M \%d \%b \%Y"), filename = "Output\\\\Reports\\\\Report.doc",
  path_string = "PATH", meta_table_string = "meta_table",
  table_path = "Output\\\\Core\\\\", figure_format = c("png", "jpeg", "ps"),
  figure_path = "Output\\\\Figures\\\\")
}
\arguments{
\item{report_title}{text string used to label the report title page}

\item{author}{text string naming the author}

\item{datestamp}{text used to give the date-time stampe, defaults to the system date/time at the time of running the function}

\item{filename}{text string giving the filename/path to output the word document to}

\item{path_string}{character string of the name of a global variable that contains the project filepath. Default is "PATH".}

\item{meta_table_string}{character string of the name of a global table that contains meta information on tables (title, population, number)}

\item{table_path}{text string giving the filepath to the tables}

\item{figure_path}{text string giving the filepath to the figures}
}
\description{
Produces the final xml file
}
\details{
all file paths need to use "\\" as "/" will not work in windows dos
}