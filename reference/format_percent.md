# Format number to percent

Format values to percentage format. Multiply 100 and add % symbol.

## Usage

``` r
format_percent(x, digits = 1, ...)
```

## Arguments

- x:

  Number to format percentage.

- digits:

  the desired number of digits after the decimal point (`format = "f"`)
  or *significant* digits (`format = "g"`, `= "e"` or `= "fg"`).

  Default: 2 for integer, 4 for real numbers. If less than 0, the C
  default of 6 digits is used. If specified as more than 50, 50 will be
  used with a warning unless `format = "f"` where it is limited to
  typically 324. (Not more than 15–21 digits need be accurate, depending
  on the OS and compiler used. This limit is just a precaution against
  segfaults in the underlying C runtime.)

- ...:

  arguments passed to `format`.

## Value

A formatted percent character.
