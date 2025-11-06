# Generate a descriptive summary statistics table.

It is important to use variable label and value label to produce a
proper descriptive table. Variables with value labels will be converted
to ordered factor with same order as the value labels (`to_factor`). And
variable labels will be used in the output. The first row will be blank
with row names of variable label. Variable name will be used if the
variable does not have a variable label.

## Usage

``` r
stat_tab(
  vars,
  group = NULL,
  data,
  total = TRUE,
  select = NULL,
  add_missing = TRUE,
  digits = 2,
  digits_pct = 1,
  rounding_fn = signif_pad,
  render_num = "Median [Min, Max]",
  logical_na_impute = FALSE
)
```

## Arguments

- group:

  Name of the grouping variable.

- data:

  A `data.frame` from which the variables in `vars` should be taken.

- total:

  If a "Total" column will be created (default). Specify `FALSE` to omit
  the column.

- select:

  a named vector with as many components as row-variables. Every element
  of \`select\` will be used to select the individuals to be analyzed
  for every row-variable. Name of the vector corresponds to the row
  variable, element is the selection.

- add_missing:

  If missing number and missing percentage will be reported in the
  summary table, default is \`TRUE\`. This will also produce data
  missingness report if set `TRUE`. See
  [`report_missing`](https://cam-ctu.github.io/cctu/reference/report_missing.md)
  for details.

- digits:

  An integer specifying the number of significant digits to keep,
  default is 3.

- digits_pct:

  An integer specifying the number of digits after the decimal place for
  percentages, default is 0.

- rounding_fn:

  The function to use to do the rounding. Defaults is
  [`signif_pad`](https://cam-ctu.github.io/cctu/reference/signif_pad.md).
  To round up by digits instead of significant values, set it to
  `round_pad`.

- render_num:

  A character or vector indicating which summary will be reported,
  default is "Median \[Min, Max\]". You can change this to "Median \[Q1,
  Q3\]" then the median and IQR will be reported instead of "Median
  \[Min, Max\]". Use `options(cctu_render_num = "Median [IQR]")` to set
  global options. See details
  [`render_numeric`](https://cam-ctu.github.io/cctu/reference/render_numeric.md)
  [`num_stat`](https://cam-ctu.github.io/cctu/reference/num_stat.md).

- logical_na_impute:

  Impute missing values with `FALSE` (default), `NA` keep as it is, or
  `TRUE`. The nominator for the logical vector is the number of `TRUE`.
  For `FALSE` or `TRUE`, the denominator will be all values regardless
  of missingness, but the non-missing number used as denominator for
  `NA`. Set it to `FALSE` if you want to summarise multiple choice
  variables and `NA` for Yes/No type logical variables but don't want No
  in the summary. You can used a named list in `x` and stack multiple
  choice in one category.

## Value

An object of class "cttab".
