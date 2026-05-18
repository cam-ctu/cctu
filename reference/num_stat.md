# Compute some basic descriptive statistics.

Values of type `factor`, `character` and `logical` are treated as
categorical. For logicals, the two categories are given the labels
\`Yes\` for `TRUE`, and \`No\` for `FALSE`. Factor levels with zero
counts are retained.

## Usage

``` r
num_stat(x, digits = 3, digits_pct = 1, rounding_fn = signif_pad)

cat_stat(x, digits_pct = 1)
```

## Arguments

- x:

  A vector or numeric, factor, character or logical values.

- digits:

  An integer specifying the number of significant digits to keep for
  numerical results. See `signif_pad`.

- digits_pct:

  An integer specifying the number of significant digits to keep for
  percentage.

- rounding_fn:

  The function to use to do the rounding. Defaults to
  [`signif_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md).

## Value

A list. For numeric `x`, the list contains the numeric elements:

- `N`: the number of non-missing values

- `NMISS`: the number of missing values

- `SUM`: the sum of the non-missing values

- `MEAN`: the mean of the non-missing values

- `SD`: the standard deviation of the non-missing values

- `MIN`: the minimum of the non-missing values

- `MEDIAN`: the median of the non-missing values

- `CV`: the percent coefficient of variation of the non-missing values

- `GMEAN`: the geometric mean of the non-missing values if non-negative,
  or `NA`

- `GCV`: the percent geometric coefficient of variation of the
  non-missing values if non-negative, or `NA`

- `GSD`: the geometric standard deviation of the non-missing values if
  non-negative, or `NA`

- `Q1`: the first quartile of the non-missing values (alias `q25`)

- `Q2`: the second quartile of the non-missing values (alias `q50` or
  `Median`)

- `Q3`: the third quartile of the non-missing values (alias `q75`)

- `IQR`: the inter-quartile range of the non-missing values (i.e.,
  `Q3 - Q1`)

If `x` is categorical (i.e. factor, character or logical), the list
contains a sublist for each category, where each sublist contains the
numeric elements:

- `FREQ`: the frequency count

- `PCT`: the percent relative frequency, including NA in the denominator

- `PCTnoNA`: the percent relative frequency, excluding NA from the
  denominator

- `Nall`: total count, including NA from the denominator

- `N`: total count, excluding NA from the denominator

## See also

[`signif_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)
[`round_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)

## Examples

``` r
x <- exp(rnorm(100, 1, 1))
num_stat(x)
#> $N
#> [1] "100"
#> 
#> $NMISS
#> [1] "0"
#> 
#> $SUM
#> [1] "494"
#> 
#> $MEAN
#> [1] "4.94"
#> 
#> $SD
#> [1] "6.55"
#> 
#> $CV
#> [1] "132.7%"
#> 
#> $GMEAN
#> [1] "2.92"
#> 
#> $GCV
#> [1] "142.0%"
#> 
#> $GSD
#> [1] "2.86"
#> 
#> $MEDIAN
#> [1] "2.98"
#> 
#> $MIN
#> [1] "0.199"
#> 
#> $Q1
#>    25% 
#> "1.90" 
#> 
#> $Q2
#>    50% 
#> "2.98" 
#> 
#> $Q3
#>    75% 
#> "5.07" 
#> 
#> $IQR
#>    75% 
#> "3.17" 
#> 
#> $MAX
#> [1] "42.8"
#> 

y <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
y[1:10] <- NA
cat_stat(y)
#> $Female
#> $Female$FREQ
#> [1] 45
#> 
#> $Female$PCT
#> [1] "45.5%"
#> 
#> $Female$PCTnoNA
#> [1] "50.6%"
#> 
#> $Female$Nall
#> [1] 99
#> 
#> $Female$N
#> [1] 89
#> 
#> 
#> $Male
#> $Male$FREQ
#> [1] 44
#> 
#> $Male$PCT
#> [1] "44.4%"
#> 
#> $Male$PCTnoNA
#> [1] "49.4%"
#> 
#> $Male$Nall
#> [1] 99
#> 
#> $Male$N
#> [1] 89
#> 
#> 
cat_stat(is.na(y))
#> $Yes
#> $Yes$FREQ
#> [1] 10
#> 
#> $Yes$PCT
#> [1] "10.1%"
#> 
#> $Yes$PCTnoNA
#> [1] "10.1%"
#> 
#> $Yes$Nall
#> [1] 99
#> 
#> $Yes$N
#> [1] 99
#> 
#> 
#> $No
#> $No$FREQ
#> [1] 89
#> 
#> $No$PCT
#> [1] "89.9%"
#> 
#> $No$PCTnoNA
#> [1] "89.9%"
#> 
#> $No$Nall
#> [1] 99
#> 
#> $No$N
#> [1] 99
#> 
#> 
```
