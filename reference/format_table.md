# Stamp row-style metadata on a table for \`write_table()\`.

Generic helper that attaches a \`row_style\` attribute to a data.frame
or matrix so it can be rendered as a styled Word table by
\[write_table()\] / \[styled_table()\]. Each argument takes integer row
indices for the rows that should carry the corresponding token; the
helper assembles them into the per-row \`;\`-joined character vector
that the renderer consumes.

## Usage

``` r
format_table(
  x,
  bold = NULL,
  bgcol = NULL,
  bgcol_color = NULL,
  span = NULL,
  indent = NULL,
  col = NULL,
  col_color = NULL
)
```

## Arguments

- x:

  A data.frame or matrix.

- bold, bgcol, span, indent:

  Integer vectors of row indices (1-based) that should carry the
  corresponding style. \`NULL\` (default) means no rows of that style.
  Indices outside \`\[1, nrow(x)\]\` or \`NA\` are rejected with an
  error.

- bgcol_color:

  A color for the background of rows in \`bgcol\`. Accepts any R color
  name (e.g. \`"red"\`) or a 6-digit hex string (e.g. \`"FF0000"\`).
  \`NULL\` (default) uses the document default grey (\`d3d3d3\`).

- col:

  Integer vector of row indices whose text should be coloured.

- col_color:

  A color for the text of rows in \`col\`. Same format as
  \`bgcol_color\`. Required when \`col\` is non-\`NULL\`.

## Value

\`x\`, unchanged in shape, with \`attr(x, "row_style")\` set to a
character vector of length \`nrow(x)\`.

## Details

Tokens (the renderer's vocabulary): \`"bold"\` (bold text), \`"bgcol"\`
(grey background - typically a banner row), \`"span"\` (first cell spans
all columns - typically a header / banner row), \`"indent"\` (visual
indent - typically a stat row sitting under a header). A row may carry
any combination, e.g. a banner row appears in \`bold\`, \`bgcol\`, and
\`span\`. Token order in the assembled string is fixed:
\`bold;bgcol;span;indent;col\`.

## See also

\[styled_table()\], \[write_table()\], \[cttab_format()\].

## Examples

``` r
df <- data.frame(label = c("Group A", "n", "mean"), value = c("", "10", "3.4"))
df <- format_table(df, bold = 1, bgcol = 1, span = 1, indent = 2:3)
attr(df, "row_style")
#> [1] "bold;bgcol;span" "indent"          "indent"         
# [1] "bold;bgcol;span" "indent"          "indent"

# Custom background and text colors
df2 <- format_table(df, bgcol = 1, bgcol_color = "steelblue", col = 2:3, col_color = "red")
attr(df2, "row_style")
#> [1] "bgcol:4682B4" "col:FF0000"   "col:FF0000"  
```
