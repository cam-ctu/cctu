# Copy variable label and value labels

`copy_lab` copy the variable label and value labels or returns `old_var`
to variable `new_var` and return `new_var`.

## Usage

``` r
copy_lab(new_var, old_var, strict = TRUE)
```

## Arguments

- new_var:

  Variable to be copied to.

- old_var:

  Variable to be copied from.

- strict:

  Should the variables should be the same `mode`.

## Value

`copy_lab` returns `new_var` with same variable label and value labels
as `old_var`.

## Examples

``` r
var_with_lab <- rep(1:2, 5)
var_lab(var_with_lab) <- "Income"
val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)
var_nolab <- rep(1:2, 10)
var_ut <- copy_lab(var_nolab, var_with_lab)
```
