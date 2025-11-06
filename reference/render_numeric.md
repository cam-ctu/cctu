# Render continuous values for table output.

Called from [`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
by default to render continuous (i.e. `numeric`) values for displaying
in the table.

## Usage

``` r
render_numeric(x, what = "Median [Min, Max]", ...)
```

## Arguments

- x:

  A numeric vector.

- what:

  A character or vector what will be reported for numeric variables,
  default is median and min, max. See
  [`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md) for
  the supported statistics. The provided name will be used as statistic
  names (printed in the table), and the values is the statistics that
  will be reported. If you want to have a fancy name, use a named vector
  with names as the statistics will be printed and values are the
  statistics. For example,
  `c("Geo. Mean (Geo. CV%)" = "GMean (GCV)", "Median [IQR]" = "Median [Q1, Q3]")`.
  In the latter case, the user visible statistics will be called
  `"Median [IQR]"` and this is the name printed. But the underlying
  summarised statistics are median, Q1 and Q3. Separate statistics with
  comma or a space or any non-letters.

- ...:

  Further arguments, passed to
  [`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md).

## Value

A `character` vector. Each element is to be displayed in a separate cell
in the table. The [`names`](https://rdrr.io/r/base/names.html) of the
vector are the labels to use in the table.

## Details

This function was used by `link{cttab}` to render numeric variables. It
essentially uses the values returned by
[`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md) and
put values to a vector.

## See also

[`signif_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)
[`round_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)
[`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md)

## Examples

``` r
x <- exp(rnorm(100, 1, 1))
render_numeric(x)
#>           Valid Obs.            Mean (SD)    Median [Min, Max] 
#>                "100"        "4.59 (4.97)" "2.83 [0.308, 23.5]" 
```
