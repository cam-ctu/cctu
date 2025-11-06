# Replace vector/matrix/data.frame values with corresponding value labels.

`lab2val` replaces vector/matrix/data.frame values with corresponding
value labels. If there are no labels for some values they are converted
to characters in most cases. If there are no labels at all for variable
it remains unchanged.

## Usage

``` r
lab2val(x)
```

## Arguments

- x:

  vector/matrix/data.frame

## Value

Object of the same form as x but with value labels instead of values.

## References

This is a modified version from \`expss\` package.

## See also

[val_lab](https://cam-ctu.github.io/cctu/reference/val_lab.md),
[var_lab](https://cam-ctu.github.io/cctu/reference/var_lab.md)

## Examples

``` r
data(mtcars)
mtcars <- within(mtcars, {
  var_lab(mpg) <- NULL
  val_lab(am) <- c(" automatic" = 0, " manual" = 1)
})

table(lab2val(mtcars$am))
#> 
#>  automatic     manual 
#>         19         13 

summary(lm(mpg ~ ., data = lab2val(mtcars[, c("mpg", "am")])))
#> 
#> Call:
#> lm(formula = mpg ~ ., data = lab2val(mtcars[, c("mpg", "am")]))
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -9.3923 -3.0923 -0.2974  3.2439  9.5077 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   17.147      1.125  15.247 1.13e-15 ***
#> am manual      7.245      1.764   4.106 0.000285 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 4.902 on 30 degrees of freedom
#> Multiple R-squared:  0.3598, Adjusted R-squared:  0.3385 
#> F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285
#> 
```
