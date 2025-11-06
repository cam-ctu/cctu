# Check if All Elements in Character Vector are Numeric

Tests, without issuing warnings, whether all elements of a character
vector are legal numeric values, or optionally converts the vector to a
numeric vector. Leading and trailing blanks in x are ignored.

## Usage

``` r
all_is_numeric(x, extras = c(".", NA))
```

## Arguments

- x:

  a character vector

- extras:

  a vector of character strings to count as numeric values, other than
  "".

## Value

a logical value

## Examples

``` r
all_is_numeric(c("1", "1.2", "3"))
#> [1] TRUE
all_is_numeric(c("1", "1.2", "3a"))
#> [1] FALSE
```
