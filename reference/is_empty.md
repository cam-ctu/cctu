# Check whether string, list or vector is empty

This function checks whether a string or character vector, a list or any
vector (numeric, atomic) is empty or not.

## Usage

``` r
is_empty(x, na_empty = TRUE)
```

## Arguments

- x:

  String, character vector, list, data.frame or numeric vector or
  factor.

- na_empty:

  Logical, if `NA` is considered as empty (default is `TRUE`).

## Value

Logical vector.

## Examples

``` r
is_empty("test")
#>  test 
#> FALSE 
is_empty("")
#>      
#> TRUE 
is_empty(NA)
#> [1] TRUE
is_empty(NULL)
#> [1] TRUE

# string is not empty
is_empty(" ")
#>       
#> FALSE 

# however, this trimmed string is
is_empty(trimws(" "))
#>      
#> TRUE 

# numeric vector
x <- 1
is_empty(x)
#> [1] FALSE
x <- x[-1]
is_empty(x)
#> [1] TRUE

# check multiple elements of character vectors
is_empty(c("", "a"))
#>           a 
#>  TRUE FALSE 

# empty data frame
d <- data.frame()
is_empty(d)
#> [1] TRUE

# empty list
# is_empty(list(NULL))

# NA vector
x <- rep(NA, 5)
is_empty(x)
#> [1] TRUE TRUE TRUE TRUE TRUE
is_empty(x, na_empty = FALSE)
#> [1] FALSE FALSE FALSE FALSE FALSE
```
