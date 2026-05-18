# Generate a descriptive summary statistics table.

Internal long-format summariser that powers
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md). Variable
value labels are honoured (variables with labels are converted to
ordered factors). Variable labels are used in the output when present;
the variable name is used otherwise.

## Usage

``` r
stat_tab(
  vars,
  group = NULL,
  row_split = NULL,
  data,
  total = TRUE,
  add_obs = FALSE,
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

  Name of the grouping variable, length 0/1. When supplied each level
  becomes a column in the rendered table (plus an extra "Total" column
  if `total = TRUE`).

- row_split:

  Variable used for splitting table rows, length 0/1.

- data:

  A `data.frame` from which the variables in `vars` should be taken.

- total:

  If a "Total" column will be created (default). Specify `FALSE` to omit
  the column.

- add_obs:

  Add an observation row (default).

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

A long-format `data.table`. `NULL` when there is nothing to summarise.

## Details

The returned `data.table` has the following columns:

- the row-split variable, when `row_split` is supplied;

- the grouping variable when `group` is supplied (a factor whose levels
  finish with `"Total"` when `total = TRUE`);

- `Group_ID`, `Group_Label`: identifier and (optional) banner label
  produced when `vars` is a named list;

- `Var_ID`, `Variable`: per-variable id (0 = "Observation" row) and the
  rendered variable label;

- `Stat_ID`, `Statistic`: per-statistic id and label;

- `Value`: the rendered cell value (character)
