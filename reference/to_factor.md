# Convert vector but keep value/value labels.

Convert vector but keep value/value labels.

`to_factor` convert vector to factor with corresponding value labels or
unique values in the vector. Note that the \`NA\` or blank values will
be excluded from converting. It will return a factor with original
values if there's no value label.

`to_character` Convert vector to character vector and keep the variable
labels. The generic function \`as.character\` will lost the label
attributes. If you want to convert the value label to value itself, use
`lab2val` instead.

`to_numeric` Convert vector to numeric vector and keep the variable
labels. The generic function \`as.numeric\` will lost the label
attributes.

`to_logical` Convert vector to logical vector and keep the variable
labels. The generic function \`as.logical\` will lost the label
attributes.

## Usage

``` r
to_factor(x, ordered = TRUE, drop_levels = FALSE)

to_character(x)

to_numeric(x)

to_logical(x)
```

## Arguments

- x:

  vector

- ordered:

  logical flag to determine if the levels should be regarded as ordered.

- drop_levels:

  If \`TRUE\`, drop unused levels from the converted factor, default is
  \`FALSE\`. This argument has previously been "drop.levels".

## Value

Factor/character of the same form as x but with value labels instead of
values. For numeric and logical vectors with corresponding type.

## See also

[val_lab](https://cam-ctu.github.io/cctu/reference/val_lab.md),
[var_lab](https://cam-ctu.github.io/cctu/reference/var_lab.md)

## Examples

``` r
data(mtcars)
#> name=mtcars:  NOT found in names() of Rdata.rds, i.e.,
#>  cancer,cgd,diabetic,flchain,heart,logan,nafld,nwtco,pbc,reliability,retinopathy,rhDNase,solder,survexp,tobin,transplant,udca
#> name=mtcars:  NOT found in names() of Rdata.rds, i.e.,
#>  diamonds,economics,economics_long,faithfuld,luv_colours,midwest,mpg,msleep,presidential,seals,txhousing
#> name=mtcars:  NOT found in names() of Rdata.rds, i.e.,
#>  meta_table_example
#> name=mtcars:  found in Rdata.rds
mtcars <- within(mtcars, {
  var_lab(am) <- "Transmission"
  val_lab(am) <- c(" automatic" = 0, " manual" = 1)
})

mtcars$am <- to_factor(mtcars$am)
mtcars$gear <- to_character(mtcars$gear)
```
