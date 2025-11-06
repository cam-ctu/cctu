# Round numbers with 0-padding.

Utility functions to round numbers, similar the the base functions
`signif` and `round`, but resulting in character representations that
keep zeros at the right edge if they are significant.

## Usage

``` r
signif_pad(
  x,
  digits = 3,
  round.integers = TRUE,
  round5up = TRUE,
  format = "fg",
  ...
)

round_pad(x, digits = 2, round5up = TRUE, ...)
```

## Arguments

- x:

  A numeric vector.

- digits:

  An integer specifying the number of significant digits to keep (for
  `signif_pad`) or the number of digits after the decimal point (for
  `round_pad`).

- round.integers:

  Should rounding be limited to digits to the right of the decimal
  point?

- round5up:

  Should numbers with 5 as the last digit always be rounded up? The
  standard R approach is "go to the even digit" (IEC 60559 standard, see
  [`round`](https://rdrr.io/r/base/Round.html)), while some other
  softwares (e.g. SAS, Excel) always round up.

- format:

  See `formatC`. Default is `"fg"`, change it to `"g"` if scientific is
  preferred.

- ...:

  Further options, passed to `formatC` (which is used internally). Not
  all options will work, but some might be useful (e.g. `big.mark`,
  `decimal.mark`).

## Value

A character vector containing the rounded numbers.

## References

This is a direct copy from \`table1\` package

## See also

[`signif`](https://rdrr.io/r/base/Round.html)
[`round`](https://rdrr.io/r/base/Round.html)
[`formatC`](https://rdrr.io/r/base/formatc.html)
[`prettyNum`](https://rdrr.io/r/base/formatc.html)
[`format`](https://rdrr.io/r/base/format.html)

## Examples

``` r
x <- c(0.9001, 12345, 1.2, 1., 0.1, 0.00001, 1e5)
signif_pad(x, digits = 3)
#> [1] "0.900"     "12300"     "1.20"      "1.00"      "0.100"     "0.0000100"
#> [7] "100000"   
signif_pad(x, digits = 3, round.integers = TRUE)
#> [1] "0.900"     "12300"     "1.20"      "1.00"      "0.100"     "0.0000100"
#> [7] "100000"   
round_pad(x, digits = 2)
#> [1] "0.90"      "12345.00"  "1.20"      "1.00"      "0.10"      "0.00"     
#> [7] "100000.00"

# Compare:
as.character(signif(x, digits = 3))
#> [1] "0.9"   "12300" "1.2"   "1"     "0.1"   "1e-05" "1e+05"
format(x, digits = 3, nsmall = 3)
#> [1] "9.00e-01" "1.23e+04" "1.20e+00" "1.00e+00" "1.00e-01" "1.00e-05" "1.00e+05"
prettyNum(x, digits = 3, drop0trailing = TRUE)
#> [1] "0.9"   "12345" "1.2"   "1"     "0.1"   "1e-05" "1e+05"
prettyNum(x, digits = 3, drop0trailing = FALSE)
#> [1] "0.9"   "12345" "1.2"   "1"     "0.1"   "1e-05" "1e+05"

# This is very close.
formatC(x, format = "fg", flag = "#", digits = 3)
#> [1] "0.900"     "12345."    "1.20"      "1.00"      "0.100"     "0.0000100"
#> [7] "100000."  
formatC(signif(x, 3), format = "fg", flag = "#", digits = 3)
#> [1] "0.900"     "12300."    "1.20"      "1.00"      "0.100"     "0.0000100"
#> [7] "100000."  

# Could always remove the trailing "."
sub("[.]$", "", formatC(x, format = "fg", flag = "#", digits = 3))
#> [1] "0.900"     "12345"     "1.20"      "1.00"      "0.100"     "0.0000100"
#> [7] "100000"   
```
