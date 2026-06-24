# Render a styled table to Word-flavoured XML.

Generic XML renderer for any data.frame / matrix that carries a
`row_style` attribute. `row_style` is a per-row character vector of
`;`-joined tokens drawn from
`{"bold", "bgcol", "span", "indent", "col"}`; `""` marks a plain stat
row. Tokens are passed through verbatim into the `style` attribute on
each `<td>`, where the XSLT in `inst/assets/document.xslt` (used by
[`write_docx`](https://cam-ctu.github.io/cctu/reference/write_docx.md))
interprets them.

## Usage

``` r
styled_table(x, spanner_sep = NULL)
```

## Arguments

- x:

  A data.frame (with a `label` column) or matrix.

- spanner_sep:

  `NULL` for a single-row header, or a separator string passed to
  [`build_thead`](https://cam-ctu.github.io/cctu/reference/build_thead.md)
  to build two-level spanner headers from the data-column names.

## Value

A character scalar containing the table's XML fragment.

## Details

Two input shapes are accepted:

- a `data.frame` with a `label` column (the first column of the rendered
  table) plus one column per data column;

- a `matrix` whose row-names act as labels.
