# Summarise subject-level events by class and term

Generic counter for AE-style tables: counts unique subjects at the class
level, term level, and overall, optionally stratified by treatment and
grade/severity. Works for adverse events, medical history, prior/
concomitant medications, or anything with a class/term hierarchy.

## Usage

``` r
ae_summary(
  data,
  adsl,
  id_var = cctu_opt("subjid_string"),
  term_var,
  class_var = NULL,
  trt_var = NULL,
  overall = TRUE,
  grade_var = NULL,
  grades = NULL,
  grade_layout = c("vertical", "horizontal"),
  grade_count = c("worst", "any"),
  grade_report = c("all", "high"),
  grade_high = 3L,
  min_pct = 0,
  class_sort = c("freq", "alpha"),
  any_label = "Participants with any event"
)
```

## Arguments

- data:

  Event-level dataset (one row per event).

- adsl:

  Subject-level dataset providing denominators.

- id_var:

  Subject ID column name (string). Must exist in both \`data\` and
  \`adsl\`.

- term_var:

  Preferred term column name in \`data\` (string).

- class_var:

  Class/SOC column name in \`data\` (string), or \`NULL\`. When
  \`NULL\`, the table is flat: a top "any event" row followed by
  term-level rows, with no class grouping.

- trt_var:

  Optional treatment column in both \`adsl\` and \`data\`.

- overall:

  Add a Total column when \`trt_var\` is given.

- grade_var:

  Optional grade/severity column in \`data\`.

- grades:

  Grade levels to report; defaults to "Any grade" + observed. When
  supplied, the order is taken as ascending severity (worst last), which
  also drives worst-grade counting.

- grade_layout:

  When \`grade_var\` is given, "vertical" (default) nests grades as rows
  beneath each term: the term row shows the any-grade count and one
  indented sub-row per grade group gives the subject count for that
  group. "horizontal" instead puts each grade group in its own column.
  Class/overall rows stay any-grade. No effect when \`grade_var\` is
  \`NULL\`.

- grade_count:

  How a subject is counted across grades for a given term (or class).
  "worst" (default) counts each subject once, at their maximum grade, so
  grade rows are mutually exclusive and sum to the any-grade count.
  "any" counts a subject at every grade they experienced (rows may sum
  past the any-grade count). Worst-grade is taken independently at each
  level.

- grade_report:

  Which grade groups to show. "all" (default) reports each grade. "high"
  reports a single collapsed "Grade \>=k" group (k = \`grade_high\`)
  alongside the any-grade row. Requires numeric grade labels (e.g.
  "Grade 3", "3").

- grade_high:

  Threshold k for \`grade_report = "high"\`; default 3 gives a "Grade
  \>=3" group.

- min_pct:

  Term-level frequency filter (percent); 0 disables.

- class_sort:

  "freq" (default) or "alpha".

- any_label:

  Label for the top "any event" row. Defaults to "Subjects with any
  event"; pass e.g. "Patients with any AE" or "Subjects with any medical
  history" to match context.

## Value

A `data.frame` of class `cttab`: a `label` column, one column per arm
(or arm x grade group), and a `row_style` attribute (via
[`format_table`](https://cam-ctu.github.io/cctu/reference/format_table.md))
that bolds the any-event and class rows - and, in the vertical grade
layout, the term rows that carry nested grade sub-rows - and indents the
term / grade rows for
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md).

## See also

[`format_table`](https://cam-ctu.github.io/cctu/reference/format_table.md),
[`shift_table`](https://cam-ctu.github.io/cctu/reference/shift_table.md),
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)

## Examples

``` r
adsl <- data.frame(id = sprintf("S%02d", 1:10),
                   arm = rep(c("Active", "Placebo"), each = 5))
ae <- data.frame(
  id  = c("S01", "S01", "S02", "S03", "S06", "S07"),
  soc = c("Skin", "GI", "Skin", "Skin", "GI", "Skin"),
  pt  = c("Rash", "Nausea", "Rash", "Pruritus", "Nausea", "Rash"),
  gr  = c("Grade 1", "Grade 3", "Grade 2", "Grade 1", "Grade 2", "Grade 1")
)
# Class -> term hierarchy with worst-grade sub-rows and a Total column.
ae_summary(ae, adsl, id_var = "id", class_var = "soc", term_var = "pt",
           trt_var = "arm", grade_var = "gr")
#> Warning: Both 'by_cols' and '..by_cols' exist in calling scope. Please remove the '..by_cols' variable in calling scope for clarity.
#> Warning: Both 'by_cols' and '..by_cols' exist in calling scope. Please remove the '..by_cols' variable in calling scope for clarity.
#> ┌───────────────────────────┬────────┬────────┬────────┐
#> |                           │ Active │Placebo │ Total  |
#> ├───────────────────────────┴────────┴────────┴────────┤
#> |Participants with any event│3 (60.0)│2 (40.0)│5 (50.0)|
#> |Skin                       │3 (60.0)│1 (20.0)│4 (40.0)|
#> |     Rash                  │2 (40.0)│1 (20.0)│3 (30.0)|
#> |        Grade 1            │1 (20.0)│1 (20.0)│2 (20.0)|
#> |        Grade 2            │1 (20.0)│   0    │1 (10.0)|
#> |        Grade 3            │   0    │0 (0.0) │   0    |
#> |     Pruritus              │1 (20.0)│   0    │1 (10.0)|
#> |        Grade 1            │1 (20.0)│   0    │1 (10.0)|
#> |        Grade 3            │   0    │0 (0.0) │   0    |
#> |GI                         │1 (20.0)│1 (20.0)│2 (20.0)|
#> |     Nausea                │1 (20.0)│1 (20.0)│2 (20.0)|
#> |        Grade 2            │   0    │1 (20.0)│1 (10.0)|
#> |        Grade 3            │1 (20.0)│0 (0.0) │1 (10.0)|
#> └───────────────────────────┴────────┴────────┴────────┘

# Flat table (no class): a single bold any-event row over indented terms.
ae_summary(ae, adsl, id_var = "id", term_var = "pt", trt_var = "arm")
#> ┌───────────────────────────┬────────┬────────┬────────┐
#> |                           │ Active │Placebo │ Total  |
#> ├───────────────────────────┴────────┴────────┴────────┤
#> |Participants with any event│3 (60.0)│2 (40.0)│5 (50.0)|
#> |     Rash                  │2 (40.0)│1 (20.0)│3 (30.0)|
#> |     Nausea                │1 (20.0)│1 (20.0)│2 (20.0)|
#> |     Pruritus              │1 (20.0)│   0    │1 (10.0)|
#> └───────────────────────────┴────────┴────────┴────────┘
```
