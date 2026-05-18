# Drop rows with missing values in any grouping / row-split column.

Returns a new \`data.table\` (does not mutate \`data\` in place). Emits
a \`message()\` listing the dropped count when any rows are removed so
the user is aware their group / row-split column carried \`NA\`s.

## Usage

``` r
cttab_drop_na_grouping(data, cols)
```

## Arguments

- data:

  A \`data.table\`.

- cols:

  Column names to check. \`NULL\` entries are silently skipped so
  \`c(group, row_split)\` works when one is \`NULL\`.

## Value

A \`data.table\` with the offending rows removed.
