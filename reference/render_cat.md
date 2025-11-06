# Render categorical values for table output.

Called from [`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
by default to render categorical (i.e. `factor`, `character` or
`logical`) values for displaying in the table.

## Usage

``` r
render_cat(x, ...)
```

## Arguments

- x:

  A vector of type `factor`, `character` or `logical`.

- ...:

  Further arguments, passed to
  [`cat_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md).

## Value

A `character` vector. Each element is to be displayed in a separate cell
in the table. The [`names`](https://rdrr.io/r/base/names.html) of the
vector are the labels to use in the table.

## Details

This function was used by `link{cttab}` to render categorical variables.
It essentially uses the values returned by
[`cat_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md) and
put values to a vector. You can modified this to show any values you
want, checkout the example below.

## See also

[`signif_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)
[`round_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md)
[`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md)

## Examples

``` r
y <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
y[1:10] <- NA
render_cat(y)
#>          Female            Male 
#> "36/89 (40.4%)" "53/89 (59.6%)" 
```
