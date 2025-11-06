# Format p-value

Format p-value

## Usage

``` r
format_pval(pvals, sig_limit = 10^(-digits), digits = 3)
```

## Arguments

- pvals:

  A numeric value or vector of p-values

- sig_limit:

  Lower bound for precision; smaller values will be shown as \<
  sig_limit

- digits:

  Number of digits past the decimal point to keep

## Examples

``` r
pv <- c(-1, 0.00001, 0.0042, 0.0601, 0.1335, 0.4999, 0.51, 0.89, 0.9, 1)
format_pval(pv)
#>  [1] "<0.001" "<0.001" "0.004"  "0.060"  "0.134"  "0.500"  "0.510"  "0.890" 
#>  [9] "0.900"  "1.000" 
```
