# Apply DLU/CLU file to the data frame

The variable label attribute will be applied to the data frame from the
DLU file.

## Usage

``` r
apply_macro_dict(
  data,
  dlu,
  clu = NULL,
  date_format = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"),
  clean_names = TRUE,
  rm_empty = getOption("cctu_rm_empty", default = "both"),
  check_catvar = FALSE
)
```

## Arguments

- data:

  Data frame to be applied.

- dlu:

  Data frame of DLU, see
  [`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md) for
  the requirements of DLU.

- clu:

  Data frame of CLU, see details for the requirements of CLU.

- date_format:

  Date format that will be tried by
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html), default is
  `c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")`. You can add other formats you
  may want to try. The conversion will be skipped if the formatting does
  not succeed.

- clean_names:

  Convert variable name to lower case (default), this will also change
  the values in the DLU as well. See
  [`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
  for details.

- rm_empty:

  Remove empty `"rows"`, `"cols"`, or `"both"` (default), or not
  `"none"`. The
  [`remove_blank_rows_cols`](https://cam-ctu.github.io/cctu/reference/remove_blank_rows_cols.md)
  function will be used to clean the empty rows and/or columns. If the
  data is large, this will take a long time, should be set to `"none"`
  in this case. Use `options(cctu_rm_empty = "none")` to set global
  options.

- check_catvar:

  Check values of the category variable (defined in the DLU file)
  contain any non-numeric values before converting variables to numeric.

## Value

A data.table object.

## Details

### Overview

This function first convert the data to a
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
This is to avoid the variable attributes dropped by base R functions.
Then it will use the dlu file to convert the data into corresponding
variable types. After the conversion of the data, variable and value
label attribute will be created for the variable, see
[`var_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)) and
[`val_lab`](https://cam-ctu.github.io/cctu/reference/val_lab.md).

User can use
[`lab2val`](https://cam-ctu.github.io/cctu/reference/lab2val.md) to
conver value labels of the data to values if the value label is desired.
If the `clean_names` is set to `TRUE`, the data name and the dl/clu will
be cleaned, including the question names in the dlu. The cleaned dlu
data will be stored in the `cctu` environment. This will further be used
by [`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md) to
populate the missing report, see
[`report_missing`](https://cam-ctu.github.io/cctu/reference/report_missing.md).
You can change this with
[`set_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md)
function, but it will not have any effect on this function, see
[`set_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md) more
details.

Please use
[`get_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md) to get
the dlu cleaned by `apply_macro_dict` or use
[`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md) to
clean it, which is the same function used by `apply_macro_dict` to clean
the DLU.

### Variable conversion based on DLU type

- IntegerData: Convert to numeric.

- Real: Convert to numeric.

- Category: If there are any non-numeric characters in the variable, no
  conversion will be performed, otherwise convert to numeric.

- Date: Convert data date format with
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html). The
  `date_format` will be used to feed the
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html) function during
  the conversion.

- Text: Convert to character.

### CLU data requirements

The CLU file contains three columns:

- ShortCode: Variable name of the downloaded data.

- CatCode: Category values, it represents the numeric code for an item
  in the CRF.

- CatValue: Label of the category values, for example.

## See also

[`var_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)
[`val_lab`](https://cam-ctu.github.io/cctu/reference/val_lab.md)
[`tidy_dlu`](https://cam-ctu.github.io/cctu/reference/tidy_dlu.md)
[`set_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md)
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
[`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
[`read_data`](https://cam-ctu.github.io/cctu/reference/read_data.md)
[`remove_blank_rows_cols`](https://cam-ctu.github.io/cctu/reference/remove_blank_rows_cols.md)
[`lab2val`](https://cam-ctu.github.io/cctu/reference/lab2val.md)
[`get_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md)

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
