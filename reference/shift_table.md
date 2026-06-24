# Baseline x worst-post-baseline shift table

Long-format safety shift table. One ordered-factor \`value\` column
supplies the categories (levels best -\> worst); all post-baseline
visits collapse into a single "worst" category. Grouping variables are
placed on the vertical axis (\`row_groups\`, stacked row blocks) or the
horizontal axis (\`col_groups\`, column spanners), e.g. PARAM down, ARM
across.

## Usage

``` r
shift_table(
  data,
  value,
  id_var = cctu_opt("subjid_string"),
  visit = "AVISIT",
  bl_value = "Baseline",
  row_groups = NULL,
  col_groups = NULL,
  worst = c("max", "min"),
  missing_baseline = "Missing",
  no_postbaseline = "No post-baseline",
  drop_no_postbaseline = FALSE,
  drop_empty = c("none", "rows", "cols", "both"),
  pct = c("row", "none", "total")
)
```

## Arguments

- data:

  long data.table: one row per subject-visit(-parameter).

- value:

  ordered-factor column holding the categories (best -\> worst).

- id_var:

  subject identifier column.

- visit:

  visit column.

- bl_value:

  value(s) of \`visit\` that mark baseline (length \>= 1).

- row_groups:

  groups spanning vertically (stacked row blocks).

- col_groups:

  groups spanning horizontally (column spanners).

- worst:

  "max" (default) or "min": which end of the ordering is worst.

- missing_baseline, no_postbaseline:

  labels for absent baseline / post.

- drop_no_postbaseline:

  drop subjects with no post-baseline assessment.

- drop_empty:

  drop empty rows/columns, evaluated on the table margins within the
  group layout: a row is (row_groups, baseline) and a column is
  (col_groups, worst); it is dropped when its marginal total is zero, so
  blocks may become ragged. "none" (default), "rows", "cols", "both".

- pct:

  "none" (default), "row" (% within baseline, per block) or "total" (%
  of the group block N).

## Value

A `data.frame` of class `cttab`: a `label` column, the count columns,
and a `row_style` attribute (via
[`format_table`](https://cam-ctu.github.io/cctu/reference/format_table.md))
that bolds the `row_groups` banner rows and indents the baseline rows
beneath them for
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md).
With no `row_groups` the rows are plain.

## Details

Contract (derivation is assumed done upstream): \* \`value\` is an
ordered factor (levels best -\> worst). \* Exactly one baseline row per
subject x group; more is an error. \* Every non-baseline row is treated
as post-baseline. \* One call is homogeneous in its level system. Mix
CTCAE grades and Low/Normal/High by calling once per system and stacking
same-level parameters via \`row_groups\`.

## See also

[`format_table`](https://cam-ctu.github.io/cctu/reference/format_table.md),
[`group_data`](https://cam-ctu.github.io/cctu/reference/group_data.md),
[`ae_summary`](https://cam-ctu.github.io/cctu/reference/ae_summary.md),
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)

## Examples

``` r
library(data.table)
#> Warning: package ‘data.table’ already present in search()
dt <- data.table(
  USUBJID = rep(sprintf("S%02d", 1:6), each = 2),
  AVISIT  = rep(c("Baseline", "Week 4"), times = 6),
  PARAM   = rep(c("ALT", "AST"), each = 6),
  ARM     = rep(c("A", "B"), each = 2, length.out = 12),
  AVALC   = c("Normal", "High", "Normal", "Normal", "High", "High",
              "Normal", "High", "Low", "Normal", "Normal", "High")
)
dt[, AVALC := factor(AVALC, levels = c("Low", "Normal", "High"),
                     ordered = TRUE)]
#>     USUBJID   AVISIT  PARAM    ARM  AVALC
#>      <char>   <char> <char> <char>  <ord>
#>  1:     S01 Baseline    ALT      A Normal
#>  2:     S01   Week 4    ALT      A   High
#>  3:     S02 Baseline    ALT      B Normal
#>  4:     S02   Week 4    ALT      B Normal
#>  5:     S03 Baseline    ALT      A   High
#>  6:     S03   Week 4    ALT      A   High
#>  7:     S04 Baseline    AST      B Normal
#>  8:     S04   Week 4    AST      B   High
#>  9:     S05 Baseline    AST      A    Low
#> 10:     S05   Week 4    AST      A Normal
#> 11:     S06 Baseline    AST      B Normal
#> 12:     S06   Week 4    AST      B   High
# PARAM as bold row-group banners (down), ARM across, baseline rows indented.
shift_table(dt, value = "AVALC", id_var = "USUBJID", visit = "AVISIT",
            bl_value = "Baseline", row_groups = "PARAM", col_groups = "ARM",
            pct = "none")
#> ┌────────────┬─────┬────────┬──────┬──────────────────┬─────┬────────┬──────┬──────────────────┐
#> |            │A_Low│A_Normal│A_High│A_No post-baseline│B_Low│B_Normal│B_High│B_No post-baseline|
#> ├────────────┴─────┴────────┴──────┴──────────────────┴─────┴────────┴──────┴──────────────────┤
#> |ALT         │     │        │      │                  │     │        │      │                  |
#> |     Low    │  0  │   0    │  0   │        0         │  0  │   0    │  0   │        0         |
#> |     Normal │  0  │   0    │  1   │        0         │  0  │   1    │  0   │        0         |
#> |     High   │  0  │   0    │  1   │        0         │  0  │   0    │  0   │        0         |
#> |     Missing│  0  │   0    │  0   │        0         │  0  │   0    │  0   │        0         |
#> |AST         │     │        │      │                  │     │        │      │                  |
#> |     Low    │  0  │   1    │  0   │        0         │  0  │   0    │  0   │        0         |
#> |     Normal │  0  │   0    │  0   │        0         │  0  │   0    │  2   │        0         |
#> |     High   │  0  │   0    │  0   │        0         │  0  │   0    │  0   │        0         |
#> |     Missing│  0  │   0    │  0   │        0         │  0  │   0    │  0   │        0         |
#> └────────────┴─────┴────────┴──────┴──────────────────┴─────┴────────┴──────┴──────────────────┘
```
