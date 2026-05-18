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
styled_table(x)
```

## Arguments

- x:

  A data.frame (with a `label` column) or matrix.

## Value

A character scalar containing the table's XML fragment.

## Details

Two input shapes are accepted:

- a `data.frame` with a `label` column (the first column of the rendered
  table) plus one column per data column;

- a `matrix` whose row-names act as labels.
