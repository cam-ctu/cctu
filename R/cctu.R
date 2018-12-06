#' cctu: A package for producing analysis reports from clinical trials
#'
#' The package `cctu` is designed to help run the analysis of clinical trials which typically produce an output
#' of a large word document with one page per table or figure following on from a Statistical Analysis Plan,
#' and requires substantial audit trails. Automating the trivialities of getting tables into word from R,
#' and capturing the meta data and audit trials is what the package achieves.
#'
#' See the vignette \code{vignette("analysis-template", package="cctu")} for an overview and starting point.
#'
#' @section Key Functions:
#'
#' \itemize{
#'  \item \code{\link{meta_table}}
#'  \item \code{\link{create_popn_envir}}
#'  \item \code{\link{attach_pop}}
#'  \item \code{\link{sumby}}
#'  \item \code{\link{write_table}}
#'  \item \code{\link{write_ggplot}}
#'  \item \code{\link{create_word_xml}}
#' }
#'
#'
#' @docType package
#' @name cctu
NULL
