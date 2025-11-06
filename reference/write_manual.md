# Function to write to meta-table and clean up without creating a table,text, or figure. For manual use

Function to write to meta-table and clean up without creating a
table,text, or figure. For manual use

## Usage

``` r
write_manual(
  number = cctu_env$number,
  clean_up = TRUE,
  verbose = options()$verbose
)
```

## Arguments

- number:

  the number used to as a suffix in the output filename, and to link to
  TableofTables. Default is to use the value in the cctu_env package
  environment that is set within
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md).

- clean_up:

  logical to invoke the
  [`clean_up`](https://cam-ctu.github.io/cctu/reference/clean_up.md)
  function at the end. Defaults to TRUE

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

## Value

Edits the meta_table object with the calling program and optionally
cleans up. No return object. Meant for use if a table or figure for a
report has been created manually outside of
[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
or
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)

## See also

[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
