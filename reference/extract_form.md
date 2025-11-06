# Extract data by form from MACRO dataset

Extract data by form from MACRO dataset. Data will be transformed to
long format adding a new column of \`visit\`.

## Usage

``` r
extract_form(
  data,
  form,
  visit = NULL,
  vars_keep = NULL,
  dlu = cctu_env$dlu,
  rm_empty = getOption("cctu_rm_empty", default = "both")
)
```

## Arguments

- data:

  A data.frame from macro dataset.

- form:

  Name of the form in the DLU file, see
  [`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md).

- visit:

  A character string or vector of visit name in the DLU file, see
  [`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md).

- vars_keep:

  Parameters to keep in the output data. This is useful if you want to
  keep treatment arm or age variable.

- dlu:

  A DLU data.frame

- rm_empty:

  Remove empty `"rows"`, `"cols"`, or `"both"` (default), or not
  `"none"`. The
  [`remove_blank_rows_cols`](https://cam-ctu.github.io/cctu/reference/remove_blank_rows_cols.md)
  function will be used to clean the empty rows and/or columns. Use
  `options(cctu_rm_empty = "none")` to set global options.

## Value

A data.table object.

## See also

[`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md)
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
[`read_data`](https://cam-ctu.github.io/cctu/reference/read_data.md)
[`remove_blank_rows_cols`](https://cam-ctu.github.io/cctu/reference/remove_blank_rows_cols.md)

## Examples

``` r
# Read MACRO data
dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"),
               colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

# Create subjid
dt$subjid <- substr(dt$USUBJID, 8, 11)

df <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)

# Following can give you the same dlu file used by apply_macro_dict
dlu <- tidy_dlu(dlu, clean_names = FALSE)

# Extract data from Lab form
lb <- extract_form(df, "Lab")

# Extract screening visit data from lab form
lb_base <- extract_form(df, "Lab", visit = "SCREENING")
```
