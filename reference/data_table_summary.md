# Provides a summary with date stamps of external data files.

Provides a summary with date stamps of external data files.

## Usage

``` r
data_table_summary(
  data_table,
  name_variable = "name",
  file_variable = "file",
  folder_variable = "folder",
  mod_time_variable = "mod_time"
)
```

## Arguments

- data_table:

  data frame containing the meta-table of file paths of the external
  data files, and their desired R object names.

- name_variable:

  character string giving the variable name within `data` that has the
  object names to be referenced. Defaults to "name".

- file_variable:

  character string giving the variable name within `data` that has the
  file names to be referenced. Defaults to "file".

- folder_variable:

  character string giving the variable name within `data` that contains
  the folder to find the external file. `data_table_summary` Looks up
  [`getwd()`](https://rdrr.io/r/base/getwd.html) if empty.

- mod_time_variable:

  character string given the variable name to be created in
  `data_table_summary` that records the time stamp of when the external
  file was last modified.

## Value

`data_table_summary` returns a data frame sumarising the meta-table and
the associated information about time of last modification and full file
paths.

## See also

[`read_data`](https://cam-ctu.github.io/cctu/reference/read_data.md)
