#' Round numbers with 0-padding.
#'
#' Utility functions to round numbers, similar the the base functions
#'  \code{signif} and \code{round}, but resulting in character representations
#'   that keep zeros at the right edge if they are significant.
#'
#' @param x A numeric vector.
#' @param digits An integer specifying the number of significant digits to keep
#' (for \code{signif_pad}) or the number of digits after the decimal point (for
#' \code{round_pad}).
#' @param round.integers Should rounding be limited to digits to the right of
#' the decimal point?
#' @param round5up Should numbers with 5 as the last digit always be rounded
#' up? The standard R approach is "go to the even digit" (IEC 60559 standard,
#' see \code{\link{round}}), while some other softwares (e.g. SAS, Excel)
#' always round up.
#' @param format See \code{formatC}. Default is \code{"fg"}, change it to
#'  \code{"g"} if scientific is preferred.
#' @param ... Further options, passed to \code{formatC} (which is used
#' internally). Not all options will work, but some might be useful (e.g.
#' \code{big.mark}, \code{decimal.mark}).
#'
#' @return A character vector containing the rounded numbers.
#'
#' @seealso
#' \code{\link{signif}}
#' \code{\link{round}}
#' \code{\link{formatC}}
#' \code{\link{prettyNum}}
#' \code{\link{format}}
#'
#' @references
#' This is a direct copy from `table1` package
#'
#' @examples
#' x <- c(0.9001, 12345, 1.2, 1., 0.1, 0.00001, 1e5)
#' signif_pad(x, digits = 3)
#' signif_pad(x, digits = 3, round.integers = TRUE)
#' round_pad(x, digits = 2)
#'
#' # Compare:
#' as.character(signif(x, digits = 3))
#' format(x, digits = 3, nsmall = 3)
#' prettyNum(x, digits = 3, drop0trailing = TRUE)
#' prettyNum(x, digits = 3, drop0trailing = FALSE)
#'
#' # This is very close.
#' formatC(x, format = "fg", flag = "#", digits = 3)
#' formatC(signif(x, 3), format = "fg", flag = "#", digits = 3)
#'
#' # Could always remove the trailing "."
#' sub("[.]$", "", formatC(x, format = "fg", flag = "#", digits = 3))
#'
#' @keywords utilities
#' @export
signif_pad <- function(x,
                       digits = 3,
                       round.integers = TRUE,
                       round5up = TRUE,
                       format = "fg",
                       ...) {
  args <- list(...)

  eps <- if (round5up) {
    x * (10^(-(digits + 3)))
  } else {
    0
  }

  rx <- ifelse(x >= 10^digits & !round.integers,
    round(x),
    signif(x + eps, digits)
  )

  c_pos <- which(x %in% c(NA, -Inf, Inf, NaN)) # Position of non-numbers

  cx <- do.call(
    formatC,
    c(
      list(
        x = rx,
        digits = digits,
        format = format,
        flag = "#"
      ),
      args[names(args) %in% names(formals(formatC))]
    )
  )

  cx[is.na(x)] <- "0" # Put in a dummy value for missing x
  cx <- gsub("[^0-9]*$", "", cx) # Remove any trailing non-digit characters
  # An exact zero is deliberately left as "0", not padded to "0.00": padding
  # would read as "a small value that rounded down to zero" (e.g. 0.0001),
  # whereas "0" is unambiguously exact.
  cx[c_pos] <- x[c_pos] # Restore non-numbers

  return(cx)
}

#' @rdname signif_pad
#' @export
round_pad <- function(x,
                      digits = 2,
                      round5up = TRUE,
                      ...) {
  args <- list(...)

  # The nudge must go AWAY from zero, as it does in signif_pad() (which scales
  # by `x`). A bare positive constant pulls negative values TOWARDS zero, so
  # round_pad(-0.135, 2) gave "-0.13" where even base round() gives -0.14 --
  # silently biasing signed values (e.g. change-from-baseline) toward zero.
  eps <- if (round5up) {
    sign(x) * 10^(-(digits + 3))
  } else {
    0
  }

  rx <- round(x + eps, digits)

  cx <- do.call(
    formatC,
    c(
      list(
        x = rx,
        digits = digits,
        format = "f",
        flag = "0"
      ),
      args[names(args) %in% names(formals(formatC))]
    )
  )
  # An exact zero renders "0", not "0.00". This also disambiguates it from a
  # genuinely small value that rounds down: round_pad(0.001, 2) is "0.00",
  # round_pad(0, 2) is "0". `which()` drops NA positions from the index.
  cx[which(x == 0)] <- "0"
  ifelse(is.na(x), NA, cx)
}

#' Format number to percent
#'
#' Format values to percentage format. Multiply 100 and add \% symbol.
#' @param x Number to format percentage.
#' @inheritParams base::formatC
#'
#' @return A formatted percent character.
#' @export
#'
format_percent <- function(x, digits = 1, ...) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  # A zero-length input must round-trip as zero-length. Without this the
  # `out[!is_empty(x)] <- res` branch below extends a length-0 `out` to
  # length 1 and returns NA.
  if (length(x) == 0L) {
    return(character(0))
  }

  res <- sapply(x[!is_empty(x)], function(v) {
    if (v == 0) {
      "0"
    } else if (v == 1) {
      "100"
    } else {
      round_pad(100 * v, digits = digits, ...)
    }
  })
  res <- paste0(res, "%")

  if (any(is_empty(x))) {
    out <- rep("", length(x))
    out[!is_empty(x)] <- res
    return(out)
  } else {
    return(res)
  }
}


#' Format p-value
#'
#' @param pvals A numeric value or vector of p-values
#' @param sig_limit Lower bound for precision; smaller values will be shown
#'  as < sig_limit
#' @param digits Number of digits past the decimal point to keep
#'
#' @export
#'
#' @examples
#' pv <- c(0.00001, 0.0042, 0.0601, 0.1335, 0.4999, 0.51, 0.89, 0.9, 1)
#' format_pval(pv)
#'
format_pval <- function(pvals, sig_limit = 10^(-digits), digits = 3) {
  out_of_range <- !is.na(pvals) & (pvals < 0 | pvals > 1)
  if (any(out_of_range)) {
    warning(
      "p-values should be in [0, 1]; ",
      sum(out_of_range), " value(s) are out of range and will be formatted as-is."
    )
  }

  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0("%.", digits, "f"), x)
    zzz <- paste0("0.", paste(rep("0", digits), collapse = ""))
    res[res == paste0("-", zzz)] <- zzz
    res
  }

  sapply(pvals, function(x, sig_limit) {
    if (is.na(x)) {
      return(x)
    }
    if (x < sig_limit) {
      sprintf("<%s", format(sig_limit, scientific = FALSE))
    } else {
      roundr(x, digits = digits)
    }
  }, sig_limit = sig_limit, simplify = TRUE)
}
