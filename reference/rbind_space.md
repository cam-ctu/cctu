# Function to bind tables/matrices with a blank row between them

Function to bind tables/matrices with a blank row between them

## Usage

``` r
rbind_space(x, y, check.names = FALSE)
```

## Arguments

- x:

  a data.frame or array, possibly of character elements only

- y:

  a data.frame or array, with the same number of columns as `x`

- check.names:

  logical. If FALSE the names are preserved from `x`, but if TRUE they
  are converted to syntactically valid variable names, which may not be
  desired for a table in a report.

## Value

a data.frame or array that is `x` stacked vertically above `y` with a
row of blank values in-between. Useful for formatting

## Details

based largely on the [`rbind`](https://rdrr.io/r/base/cbind.html)
function. Use in conjunction with
[`Reduce`](https://rdrr.io/r/base/funprog.html) to join up more than two
tables.
