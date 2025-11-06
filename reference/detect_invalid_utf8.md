# Functions to detect and delete non-UTF8 characters, which the XML output will not like

Functions to detect and delete non-UTF8 characters, which the XML output
will not like

## Usage

``` r
detect_invalid_utf8(data)

remove_invalid_utf8(data)
```

## Arguments

- data:

  a data.frame, typically the input to `write_table`

## Value

`detect_invalid_utf8` returns a data.frame that gives the column, row,
and value of any datum points that contain invalid characters, or a
zero-length data frame if none are present. `remove_invalid_utf8`
replaces any invalid characters with an empty string.

## Functions

- `remove_invalid_utf8()`: Function to delete invalid UTF8 characters

## See also

[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
