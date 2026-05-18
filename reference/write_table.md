# Function to write a table into xml format in the correct directory, and record the calling program in the meta_table.

Function to write a table into xml format in the correct directory, and
record the calling program in the meta_table.

## Usage

``` r
write_table(
  x,
  number = cctu_env$number,
  heading = NULL,
  na_to_empty = cctu_opt("na_to_empty"),
  clean_up = TRUE,
  directory = file.path(cctu_opt("output"), "Core"),
  verbose = options()$verbose,
  footnote = NULL
)
```

## Arguments

- x:

  the data.frame or table to be saved in xml format

- number:

  the number used as a suffix in the output filename, and to link to the
  meta_table. Default is to use the value in the cctu_env package
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
  function at the end. Defaults to `TRUE`.

- directory:

  where to save the table within path or current working directory.
  Defaults to `file.path(cctu_opt("output"), "Core")`.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to `options()$verbose`.

- footnote:

  character vector, can be used to add footnotes.

## Value

writes an xml version of the input data to file table_number.xml.
Records the calling program in the meta_table. No return object.

## Details

For the plain (non-styled) path, variable names and values are replaced
by variable labels and value labels respectively before writing. For a
`cttab` input the labels were already baked in upstream by
[`cttab_format`](https://cam-ctu.github.io/cctu/reference/cttab_format.md).
Set `cctu_options(na_to_empty = TRUE)` (or
`options(cctu_na_to_empty = TRUE)`) to make NA-to-empty the global
default.

## See also

[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
[`detect_invalid_utf8`](https://cam-ctu.github.io/cctu/reference/detect_invalid_utf8.md)
[`remove_invalid_utf8`](https://cam-ctu.github.io/cctu/reference/detect_invalid_utf8.md)
[`lab2val`](https://cam-ctu.github.io/cctu/reference/lab2val.md)
[`var_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)
[`val_lab`](https://cam-ctu.github.io/cctu/reference/val_lab.md)
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
