# Produce a table summarising a regression model for a study report

Produce a table summarising a regression model for a study report

## Usage

``` r
regression_table(
  x,
  labels = names(coef(x)),
  digits = getOption("cctu_digits", default = 3),
  p_digits = getOption("cctu_p_digits", default = 4),
  trans = if (class(x)[1] %in% c("glm", "coxph")) {
     exp
 } else {
     NULL
 },
  level = 0.95,
  col_names = guess_col_names(x, trans)
)
```

## Arguments

- x:

  a regression object

- labels:

  character vector describing the meaning of the coefficient parameters
  in plain English

- digits:

  integer giving the number of significant figures to print

- p_digits:

  integer giving the number of digits to print p-values, or print as
  "\<0.001" for example

- trans:

  a function to transform the coefficients by, e.g. present the odds
  ratios, as well as log-odds ratios. It intelligent tries to guess
  between no transformation and exp, but may be wrong.

- level:

  value in the unit interval to use for calculating confidence intervals

- col_names:

  character vector of the column labels. It intelligently tries to guess
  based on the class of x and the transformation, but may be wrong.

## Value

a matrix giving standard inference of coefficients, SE, confidence
intervals, p-values, plus a brief summary of the number of data points
and residual error variance.

## Details

methods exists when x is of the following classes:
`lm, glm, gls, lme, coxph, gee`. Extensions to other classes may be
written by defining methods for `coef_table` and `covar` functions

## Examples

``` r
library(survival)
#> Warning: package ‘survival’ already present in search()
cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lung)
regression_table(cfit1,
  digits = 4,
  labels = c(
    "Age (per year)", "Sex (Female vs Male)",
    "Weight loss (per pound)"
  )
)
#>                 Parameter          Log HR (SE)     HR     Conf. Int. p-value
#> 1          Age (per year)   0.02009 (0.009664)  1.020   1.001, 1.040  0.0377
#> 2    Sex (Female vs Male)     -0.5210 (0.1744) 0.5939 0.4220, 0.8359  0.0028
#> 3 Weight loss (per pound) 0.0007596 (0.006193)  1.001  0.9887, 1.013  0.9024
#> 4                                                                           
#> 5  Number of Observations                  214                              
#> 6        Number of Events                  152                              
```
