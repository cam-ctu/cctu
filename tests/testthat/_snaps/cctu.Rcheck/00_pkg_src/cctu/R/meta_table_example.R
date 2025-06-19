#' Example of a meta_table data frame
#'
#' @format a dataset with 10 columns and 3 example rows
#' \describe{
#'  \item{section}{Section label to help subdivide a report}
#'  \item{title}{title of each individual item in a repoert}
#'  \item{subtitle}{optional subtitle values. Use the empty string if no
#'  subtitle wanted.}
#'  \item{number}{the tem number, to use for link up with
#'  \code{\link{attach_pop}}, \code{\link{write_table}},
#'  \code{\link{write_ggplot}}.}
#'  \item{population}{the name of the population to use for the item. See
#'  \code{\link{create_popn_envir}}.}
#'  \item{orientation}{takes values "portrait" or "landscape" to determine the
#'  page orientation for each item}
#'  \item{margin}{takes values "normal" or "narrow" to determine the page
#'   margin for each item}
#'  \item{item}{takes values "table", "figure", or far more rarely "text".
#'   \code{\link{create_word_xml}} will expect there to exist 'table_XX.xml',
#'    'figure_XX.png' or 'text_XX.xml' as appropriate where 'XX' is the number
#'     value.}
#'  \item{footnote1}{optional footnote. Use the empty string if no subtitle
#'   wanted.}
#'  \item{footnote2}{optional footnote. Use the empty string if no subtitle
#'  wanted.}
#'  \item{fontsize}{optional fontsize. Set the font size used in a table in
#'  units 1/144 of inch. Defaults to 20 if not set.}
#'
#'
#' This is an example of the structure of a meta_table that must exist
#' internally to use \code{\link{attach_pop},\link{write_table},
#' \link{write_ggplot},\link{create_word_xml}}. All variables are characters,
#' including 'number'. The variables 'orientation' and 'item' can only take
#'  specific values.
#'
#' Typically one would create raw data using a spreadshee editor and read it
#' into R. You may add additional columns to help plan and understand what each
#'  table or figure will be, and track progress.
#'
#' See  \code{system.file("extdata", "meta_table.xlsx", package="cctu")}
#'
#' Then you must set it internally using  \code{\link{set_meta_table}}, and
#' possible examine the contents with \code{\link{get_meta_table}}
#'
#' To preserve troublesome number values '1.10', or '1.1.1' , in excel one can
#'  prefix the ' character ( to the left of the # key) before the number value
#'
#'
#' }
#'
#' @seealso \code{\link{set_meta_table}} \code{\link{get_meta_table}}
#'
"meta_table_example"
