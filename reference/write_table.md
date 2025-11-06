# Function to write a table into xml format in the correct directory, and edit TableofTables

Function to write a table into xml format in the correct directory, and
edit TableofTables

## Usage

``` r
write_table(
  x,
  number = cctu_env$number,
  heading = NULL,
  na_to_empty = getOption("cctu_na_to_empty", default = FALSE),
  clean_up = TRUE,
  directory = file.path(getOption("cctu_output", default = "Output"), "Core"),
  verbose = options()$verbose,
  footnote = NULL
)
```

## Arguments

- x:

  the data.frame or table to be saved in xml format

- number:

  the number used to as a suffix in the output filename, and to link to
  TableofTables. Default is to use the value in the cctu_env package
  environment that is set within
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md).

- heading:

  character vector of column titles. Defaults to the colnames of x

- na_to_empty:

  logical, if true then any NA values will be written as empty strings.
  Defaults to false.

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

- footnote:

  character vector, can be used to add footnotes.

## Value

writes an xml version of the input data to file table_number.xml. Edits
the TableofTables object with the calling programe. No return object.

## Details

Variable names and values will be replace by variable labels and value
labels respectively if available before writing the data. Use
`options(cctu_na_to_empty = TRUE)` to write NA values will be written as
empty strings globally.

## See also

[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
[`detect_invalid_utf8`](https://cam-ctu.github.io/cctu/reference/detect_invalid_utf8.md)
[`remove_invalid_utf8`](https://cam-ctu.github.io/cctu/reference/detect_invalid_utf8.md)
[`lab2val`](https://cam-ctu.github.io/cctu/reference/lab2val.md)
[`var_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)
[`val_lab`](https://cam-ctu.github.io/cctu/reference/val_lab.md)
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
