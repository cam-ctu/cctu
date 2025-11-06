# Set or get variable label

These functions set/get/drop variable labels. For value labels see
[val_lab](https://cam-ctu.github.io/cctu/reference/val_lab.md).

- `var_lab`:

  returns variable label or NULL if label doesn't exist.

- `var_lab<-`:

  set variable label.

- `drop_lab`:

  drops variable label.

- `has_label`:

  check if variable label exists.

## Usage

``` r
var_lab(x)

# S3 method for class 'data.frame'
var_lab(x)

var_lab(x) <- value

has_label(x)

drop_lab(x)
```

## Arguments

- x:

  Variable. In the most cases it is numeric vector.

- value:

  A character scalar - label for the variable x.

## Value

`var_lab` return variable label. If label doesn't exist it return NULL .
`var_lab<-` return variable (vector x) with attribute "label" which
equals submitted value.

## Details

Variable label is stored in attribute "label" (`attr(x,"label")`). To
drop variable label use `var_lab(var) <- NULL` or `drop_lab(var)`.

## References

This is a modified version from \`expss\` package.

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
  var_lab(mpg) <- "Miles/(US) gallon"
  var_lab(cyl) <- "Number of cylinders"
  var_lab(disp) <- "Displacement (cu.in.)"
  var_lab(hp) <- "Gross horsepower"
  var_lab(drat) <- "Rear axle ratio"
  var_lab(wt) <- "Weight (lb/1000)"
  var_lab(qsec) <- "1/4 mile time"
  var_lab(vs) <- "V/S"
  val_lab(vs) <- c("V-shaped" = 0, "straight" = 1)
  var_lab(am) <- "Transmission"
  val_lab(am) <- c(automatic = 0, manual = 1)
  var_lab(gear) <- "Number of forward gears"
  var_lab(carb) <- "Number of carburetors"
})

table(mtcars$am)
#> 
#>  0  1 
#> 19 13 
```
