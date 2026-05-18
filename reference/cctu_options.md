# Get or set cctu package options

Centralised setter / getter for the package's tunables (output
directory, default rendering, p-value digits, etc). Until now these have
been read at each call site via
`getOption("cctu_<name>", default = <D>)`, with the default literal
duplicated in every formal-argument list. `cctu_options()` replaces
those scattered defaults with one table.

## Usage

``` r
cctu_options(..., reset = FALSE)
```

## Arguments

- ...:

  Named values to set. Names must match the table above.

- reset:

  If `TRUE`, restore packaged defaults. Combined with named `...`
  arguments, the reset happens first and the new values are then applied
  on top.

## Value

When called with no arguments and `reset = FALSE`, a named list of
currently effective option values (after applying the precedence rule).
When setting, returns the *previous* values of the affected options
invisibly, in the style of
[`base::options()`](https://rdrr.io/r/base/options.html).

## Details

**Precedence.**
[`cctu_opt`](https://cam-ctu.github.io/cctu/reference/cctu_opt.md)`(name)`
resolves a value by checking, in order: (1) the base R option
`cctu_<name>` if set via
[`base::options()`](https://rdrr.io/r/base/options.html); (2) the value
stashed by `cctu_options()` in the package's internal environment; (3)
the packaged default in `.cctu_default_opts`. Setting an option via
[`base::options()`](https://rdrr.io/r/base/options.html) therefore still
wins, which preserves the existing workflow for users who set things in
`.Rprofile`.

## Available options

- `digits`:

  Significant digits for numeric summaries (default 3).

- `digits_pct`:

  Decimal places for percentages (default 0).

- `subjid_string`:

  Column name identifying a subject (default `"subjid"`).

- `print_plot`:

  Whether [`cttab()`](https://cam-ctu.github.io/cctu/reference/cttab.md)
  prints a summary plot (default `TRUE`).

- `nest`:

  How to nest variables in the output (default `"split"`) of
  [`cttab()`](https://cam-ctu.github.io/cctu/reference/cttab.md).

- `render_num`:

  Numeric statistic spec used by
  [`cttab()`](https://cam-ctu.github.io/cctu/reference/cttab.md)
  (default `"Median [Min, Max]"`).

- `blinded`:

  Suppress the grouping variable in
  [`cttab()`](https://cam-ctu.github.io/cctu/reference/cttab.md) output
  (default `FALSE`).

- `output`:

  Top-level output directory (default `"Output"`).

- `p_digits`:

  Digits for p-values (default 4).

- `rm_empty`:

  [`apply_macro_dict()`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)'s
  empty-row/column policy (default `"both"`).

- `na_to_empty`:

  Render `NA` as empty in
  [`write_table()`](https://cam-ctu.github.io/cctu/reference/write_table.md)
  (default `FALSE`).

- `source_local`:

  [`cctu::source()`](https://cam-ctu.github.io/cctu/reference/source.md)
  default for `local` (default `FALSE`).

- `fig_format`:

  Figure formats for
  [`write_ggplot()`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)
  (default `c("png", "eps")`).

## Examples

``` r
old <- cctu_options(digits = 4, p_digits = 3)
cctu_options()$digits          # 4
#> [1] 4
cctu_options(reset = TRUE)     # restore packaged defaults
do.call(cctu_options, old)     # or restore manually
```
