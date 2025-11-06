# Drop variable label and value labels

`unlab` returns variable x without variable labels and value labels

## Usage

``` r
unlab(x)
```

## Arguments

- x:

  Variable(s). Vector/data.frame/list.

## Value

`unlab` returns original variable x without variable label, value
labels.

## References

This is a modified version from \`expss\` package.

## See also

[`drop_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)
[`unval`](https://cam-ctu.github.io/cctu/reference/val_lab.md)

## Examples

``` r
raw_var <- rep(1:2, 5)
var_with_lab <- raw_var
var_lab(var_with_lab) <- "Income"
val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)
identical(raw_var, unlab(var_with_lab)) # should be TRUE
#> [1] TRUE
```
