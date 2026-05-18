# Function to write a text into xml format in the correct directory, and edit TableofTables

Function to write a text into xml format in the correct directory, and
edit TableofTables

## Usage

``` r
write_text(
  x,
  number = cctu_env$number,
  clean_up = TRUE,
  directory = file.path(cctu_opt("output"), "Core"),
  verbose = options()$verbose
)
```

## Arguments

- x:

  the character string (or vector of strings) to be saved in xml format.
  A vector is concatenated into a single text block.

- number:

  the number used as a suffix in the output filename, and to link to the
  meta_table. Default is to use the value in the cctu_env package
  environment that is set within
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md).

- clean_up:

  logical to invoke the
  [`clean_up`](https://cam-ctu.github.io/cctu/reference/clean_up.md)
  function at the end. Defaults to `TRUE`.

- directory:

  where to save the text within path or current working directory.
  Defaults to `file.path(cctu_opt("output"), "Core")`.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to `options()$verbose`.

## Value

writes an xml version of the input data to file text_number.xml . Edits
the TableofTables object with the calling programe. No return object.

## See also

[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
