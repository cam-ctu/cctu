# formats p values with rounding and \<0.001

formats p values with rounding and \<0.001

## Usage

``` r
p_format(p, digits = 3)
```

## Arguments

- p:

  vector of numeric p-values

- digits:

  the number of digits to round to, and define the threshold to use the
  \< symbol

## Value

character vector of formated p-values

## Examples

``` r
p_format(c(0.000001, 0.451234))
#> [1] "<0.001" " 0.451"
```
