# Resolve cross-references in footnote text

Replaces `@ref{number}` tokens in footnote strings with an `<xref>`
element that carries the target Word bookmark name and a display label
(e.g. `"Table 1.1"` or `"Figure 2.3"`). The XSLT in
`inst/assets/document.xslt` turns each `<xref>` into a clickable `REF`
field pointing at the bookmark that the heading template stamps on every
table/figure (`<titletype>_<digits-of-number>`). Using a `REF` field
means the reference tracks the heading number when fields are updated in
Word, rather than being frozen text. The label prefix and bookmark are
derived from the `item` column of the meta_table (`"table"` -\>
`"Table"`, `"figure"` -\> `"Figure"`; text items are rendered as
`"Table"` headings by the XSLT, so they share that bookmark prefix). If
a referenced number is not found in the meta_table a warning is issued
and the token is replaced with `"[ref: <number>]"`.

## Usage

``` r
resolve_footnote_refs(footnote, meta_table = get_meta_table())
```

## Arguments

- footnote:

  character vector of footnote strings, each potentially containing one
  or more `@ref{number}` tokens.

- meta_table:

  a data frame with at least `number` and `item` columns, as returned by
  [`get_meta_table`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md).

## Value

character vector the same length as `footnote` with all `@ref{}` tokens
replaced by `<xref>` elements.
