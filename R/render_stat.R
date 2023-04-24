
#' Render continuous values for table output.
#'
#' Called from \code{\link{cttab}} by default to render continuous (i.e.
#' \code{numeric}) values for displaying in the table.
#'
#' @param x A numeric vector.
#' @param ... Further arguments, passed to \code{\link{num_stat}}.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table.
#' @details 
#' This function was used by \code{link{cttab}} to render numeric variables.
#' It essentially uses the values returned by \code{\link{num_stat}} and 
#' put values to a vector. You can modified this to show any values you 
#' want, checkout the example below. 
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' \code{\link{num_stat}}
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' render_numeric(x)
#' 
#' # You can create your own rendering function similar to this one.
#' # Below is how to return Median and IQR
#' \dontrun{
#' my_render_numeric <- function(x, ...){
#'   with(num_stat(x, ...),
#'        c("Valid Obs." = sprintf("%s", N),
#'          "Mean (SD)"  = ifelse(is.na(MEAN), "", sprintf("%s (%s)", MEAN, SD)),
#'          "Median [IQR]" = ifelse(is.na(MEDIAN), "",
#'                                       sprintf("%s [%s]", MEDIAN, IQR))
#'        ))
#' }
#' }
#'
#' @keywords utilities
#' @export
render_numeric <- function(x, ...){
  with(num_stat(x, ...),
       c("Valid Obs." = sprintf("%s", N),
         "Mean (SD)"  = ifelse(is.na(MEAN), "", sprintf("%s (%s)", MEAN, SD)),
         "Median [Min, Max]" = ifelse(is.na(MEDIAN), "",
                                      sprintf("%s [%s, %s]", MEDIAN, MIN, MAX))
       ))
}

#' Render categorical values for table output.
#'
#' Called from \code{\link{cttab}} by default to render categorical (i.e.
#' \code{factor}, \code{character} or \code{logical}) values for displaying in the table.
#'
#' @param x A vector of type \code{factor}, \code{character} or \code{logical}.
#' @param ... Further arguments, passed to \code{\link{cat_stat}}.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table.
#' @details 
#' This function was used by \code{link{cttab}} to render categorical variables.
#' It essentially uses the values returned by \code{\link{cat_stat}} and 
#' put values to a vector. You can modified this to show any values you 
#' want, checkout the example below. 
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' \code{\link{num_stat}}
#' @examples
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render_cat(y)
#' 
#' # You can create your own rendering function similar to this one.
#' # Below is how to return Median and IQR
#' \dontrun{
#' my_render_cat <- function(x, ...){
#'   sapply(cat_stat(x, ...),
#'         function(y)
#'           with(y, ifelse(FREQ %in% c(0, NA), "",
#'                          sprintf("%s/%s (%s)", FREQ, Nall, PCTnoNA))))
#'        ))
#' }
#' }
#' 
#' @keywords utilities
#' @export
render_cat <- function(x, ...){
  sapply(cat_stat(x, ...),
         function(y)
           with(y, ifelse(FREQ %in% c(0, NA), "",
                          sprintf("%s/%s (%s)", FREQ, N, PCTnoNA))))
}

#' Compute some basic descriptive statistics.
#'
#' Values of type \code{factor}, \code{character} and \code{logical} are
#' treated as categorical. For logicals, the two categories are given the
#' labels `Yes` for \code{TRUE}, and `No` for \code{FALSE}.  Factor levels with
#' zero counts are retained.
#'
#' @param x A vector or numeric, factor, character or logical values.
#' @param digits_pct An integer specifying the number of significant digits to
#' keep for percentage.
#' @param digits An integer specifying the number of significant digits to keep
#' for numerical results. See \code{signif_pad}.
#' @param rounding_fn The function to use to do the rounding. Defaults to
#' \code{\link{signif_pad}}.
#'
#' @return A list. For numeric \code{x}, the list contains the numeric elements:
#' \itemize{
#'   \item \code{N}: the number of non-missing values
#'   \item \code{NMISS}: the number of missing values
#'   \item \code{SUM}: the sum of the non-missing values
#'   \item \code{MEAN}: the mean of the non-missing values
#'   \item \code{SD}: the standard deviation of the non-missing values
#'   \item \code{MIN}: the minimum of the non-missing values
#'   \item \code{MEDIAN}: the median of the non-missing values
#'   \item \code{CV}: the percent coefficient of variation of the non-missing values
#'   \item \code{GMEAN}: the geometric mean of the non-missing values if non-negative, or \code{NA}
#'   \item \code{GCV}: the percent geometric coefficient of variation of the
#'   non-missing values if non-negative, or \code{NA}
#'   \item \code{Q1}: the first quartile of the non-missing values (alias \code{q25})
#'   \item \code{Q2}: the second quartile of the non-missing values (alias \code{q50} or \code{Median})
#'   \item \code{Q3}: the third quartile of the non-missing values (alias \code{q75})
#'   \item \code{IQR}: the inter-quartile range of the non-missing values (i.e., \code{Q3 - Q1})
#' }
#' If \code{x} is categorical (i.e. factor, character or logical), the list
#' contains a sublist for each category, where each sublist contains the
#' numeric elements:
#' \itemize{
#'   \item \code{FREQ}: the frequency count
#'   \item \code{PCT}: the percent relative frequency, including NA in the denominator
#'   \item \code{PCTnoNA}: the percent relative frequency, excluding NA from the denominator
#'  \item \code{Nall}: total count, including NA from the denominator
#'  \item \code{N}: total count, excluding NA from the denominator
#' }
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' num_stat(x)
#'
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' cat_stat(y)
#' cat_stat(is.na(y))
#'
#' @keywords utilities
#' @export
#' @importFrom stats sd median na.omit

num_stat <- function(x, digits = 3, digits_pct = 1, rounding_fn = signif_pad){
  if(sum(!is.na(x)) == 0){
    r <- list(N = sum(!is.na(x)),
              NMISS = sum(is.na(x)),
              SUM = NA,
              MEAN = NA,
              SD = NA,
              CV = NA,
              GMEAN = NA,
              GCV = NA,
              Q1=NA,
              Q2=NA,
              Q3=NA,
              IQR=NA,
              MEDIAN = NA)
  }else{
    q <- quantile(x, probs=c(0.25, 0.5, 0.75), na.rm = TRUE)
    y <- list(N = sum(!is.na(x)),
              NMISS = sum(is.na(x)),
              SUM = sum(x, na.rm = TRUE),
              MEAN = mean(x, na.rm = TRUE),
              SD = sd(x, na.rm = TRUE),
              CV = sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)),
              GMEAN = if (any(na.omit(x) <= 0))
                NA
              else
                exp(mean(log(x), na.rm = TRUE)),
              GCV = if (any(na.omit(x) <= 0))
                NA
              else
                sqrt(exp(sd(
                  log(x), na.rm = TRUE
                ) ^ 2) - 1),
              MEDIAN = median(x, na.rm = TRUE),
              MIN = min(x, na.rm = TRUE),
              Q1=q["25%"],
              Q2=q["50%"],
              Q3=q["75%"],
              IQR=q["75%"] - q["25%"],
              MAX = max(x, na.rm = TRUE))
    cx <- lapply(y, format)

    # Percentage
    pct <- c("CV", "GCV")
    pct <- pct[pct %in% names(y)]
    pct <- pct[!is.na(y[pct])]
    r <- lapply(y, rounding_fn, digits = digits)
    r[pct] <- lapply(y[pct], format_percent, digits = digits_pct)
    r[c("N", "NMISS")] <- cx[c("N", "NMISS")]
  }
  return(r)
}


#' @rdname num_stat
#' @export
cat_stat <- function(x, digits_pct = 1){

  if (is.logical(x)) {
    x <- factor(x,
                levels = c(TRUE, FALSE),
                labels = c("Yes", "No"))
  }

  y <- table(x, useNA = "no")
  nn <- names(y)
  nn[is.na(nn)] <- "Missing"
  names(y) <- nn
  lapply(y, function(z)
    list(
      FREQ = z,
      PCT = format_percent(z / length(x), digits = digits_pct),
      PCTnoNA = format_percent(z / sum(y), digits = digits_pct),
      Nall = length(x),
      N = sum(y)
    ))
}

