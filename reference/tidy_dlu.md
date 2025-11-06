# Tidy DLU form

Separate visit, form and question into different columns. Variable
names, NOT the values, of the dlu data will be converted to lower cases.

## Usage

``` r
tidy_dlu(x, clean_names = TRUE)
```

## Arguments

- x:

  DLU data

- clean_names:

  Conver variable name to lower case (default). See
  [`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
  for details.

## Value

A data.frame with \`visit/form/question\` column separated into
\`visit\`, \`form\` and \`question\` column.

## Details

The DLU file contains four columns:

- shortcode: Variable name of the downloaded data.

- visit/form/question: Contains visit, form and question. It is
  separated by a slash. This function will separate this column into
  \`visit\`, \`form\` and \`question\` column in the output dataset. The
  \`question\` column is the unique variable name for a particular CRF
  form.

- description: description of the variable, namely variable label. This
  is will be used by
  [`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
  to create variable label.

- type: type of the variable, it has IntegerData, Text, Date, Real and
  Category four categories. This will be used by
  [`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
  to convert variables to corresponding type.

## See also

[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
[`read_data`](https://cam-ctu.github.io/cctu/reference/read_data.md)
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
[`report_missing`](https://cam-ctu.github.io/cctu/reference/report_missing.md)
[`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
