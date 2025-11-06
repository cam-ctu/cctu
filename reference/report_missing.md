# Generate missing report

This function use the same method as described in the `cctab`, reporting
missingness of the variables. It includes which form is the variable
from, set as \`Derived\` if not from DLU file. And missing percentage
with which subjects are have missing value for that particular variable.
This is internal function, not intend to use it directly by user.

## Usage

``` r
report_missing(
  data,
  vars,
  select,
  row_split = NULL,
  subjid_string = getOption("cctu_subjid_string", default = "subjid")
)
```

## Arguments

- data:

  A `data.frame` from which the variables in `vars` should be taken.

- select:

  a named vector with as many components as row-variables. Every element
  of \`select\` will be used to select the individuals to be analyzed
  for every row-variable. Name of the vector corresponds to the row
  variable, element is the selection.

- row_split:

  Variable that used for splitting table rows, rows will be split using
  this variable. Useful for repeated measures.

- subjid_string:

  A character naming the column used to identify subject, default is
  `"subjid"`.

## Value

A data frame

## Details

This requires access to a data.frame of DLU file, this will be derived
from the package environment and should be set using
[`get_dlu`](https://cam-ctu.github.io/cctu/reference/set_dlu.md).

## See also

[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
[`dump_missing_report`](https://cam-ctu.github.io/cctu/reference/dump_missing_report.md)
[`get_missing_report`](https://cam-ctu.github.io/cctu/reference/dump_missing_report.md)
[`reset_missing_report`](https://cam-ctu.github.io/cctu/reference/dump_missing_report.md)
