# Function to write a text into xml format in the correct directory, and edit TableofTables

Function to write a text into xml format in the correct directory, and
edit TableofTables

## Usage

``` r
write_text(
  x,
  number = cctu_env$number,
  clean_up = TRUE,
  directory = file.path(getOption("cctu_output", default = "Output"), "Core"),
  verbose = options()$verbose
)
```

## Arguments

- x:

  the character string to be saved in xml format

- number:

  the number used to as a suffix in the output filename, and to link to
  TableofTables. Default is to use the value in the cctu_env package
  environment that is set within
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md).

- clean_up:

  logical to invoke the
  [`clean_up`](https://cam-ctu.github.io/cctu/reference/clean_up.md)
  function at the end. Defaults to TRUE

- directory:

  where to save the figures within path or current working directory.
  The Output directory can be over-riden with options("cctu_output").

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

## Value

writes an xml version of the input data to file text_number.xml . Edits
the TableofTables object with the calling programe. No return object.

## See also

[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
