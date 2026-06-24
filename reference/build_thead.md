# Build the \`\<thead\>\` fragment, optionally with two-level spanner headers.

Shared by both the plain (`table_data`) and styled
([`styled_table`](https://cam-ctu.github.io/cctu/reference/styled_table.md))
write paths so the header logic lives in one place. With
`spanner_sep = NULL` it emits today's single-row header, byte-for-byte.
With a separator string it splits each data-column name on its first
occurrence of `spanner_sep` into a (group, leaf) pair and emits a
two-row header: an arm/group spanner row above the leaf row, with the
first (stub) column vertically merged across both rows.

## Usage

``` r
build_thead(headings, spanner_sep = NULL)
```

## Arguments

- headings:

  character vector of column headings. The first element is the
  row-label/stub and is never split.

- spanner_sep:

  `NULL` (single-row header) or a separator string used to split the
  data-column names.

## Value

a character scalar: the full `<thead>...</thead>\n` fragment.
