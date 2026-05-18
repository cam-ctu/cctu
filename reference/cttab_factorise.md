# Coerce labelled / non-factor grouping columns to factors in place.

Group / row-split columns produced by \`apply_macro_dict()\` are
typically labelled-numeric. ggplot will treat them as continuous (with
"Continuous x aesthetic" / "fill aesthetic dropped" warnings on boxplots
and dodged bars), and \`as.character()\` on the raw values returns
numeric codes rather than the labels we want in the missingness report's
\`visit\` column. Coercing once via \[to_factor()\] fixes both at the
source.

## Usage

``` r
cttab_factorise(data, cols, drop_levels = FALSE)
```

## Arguments

- data:

  A \`data.table\`.

- cols:

  Character vector of column names. \`NULL\` entries are silently
  skipped, so \`c(group, row_split)\` works even when one is \`NULL\`.

- drop_levels:

  Forwarded to \[to_factor()\].

## Details

Mutates \`data\` by reference via \[data.table::set()\] - pass a
\`copy()\` if the caller's \`data.table\` must remain untouched.
