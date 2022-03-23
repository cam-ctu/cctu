
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
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' render_numeric(x)
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
#'
#' @examples
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render_cat(y)
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
#' }
#' If \code{x} is categorical (i.e. factor, character or logical), the list
#' contains a sublist for each category, where each sublist contains the
#' numeric elements:
#' \itemize{
#'   \item \code{FREQ}: the frequency count
#'   \item \code{PCT}: the percent relative frequency, including NA in the denominator
#'   \item \code{PCTnoNA}: the percent relative frequency, excluding NA from the denominator
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
              MEDIAN = NA)
  }else{
    r <- list(N = sum(!is.na(x)),
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
              MAX = max(x, na.rm = TRUE))
    cx <- lapply(r, format)
    pct <- lapply(r[c("CV", "GCV")], format_percent, digits = digits_pct)
    r <- lapply(r, rounding_fn, digits = digits)
    r[c("N", "NMISS")] <- cx[c("N", "NMISS")]
    r[c("CV", "GCV")] <- pct
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

