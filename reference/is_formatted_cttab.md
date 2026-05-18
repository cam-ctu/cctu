# Has \`x\` been through \`cttab_format()\` already?

Centralises the "is this already a rendering-ready cttab?" check that
was duplicated across \`cttab_format()\`, \`print.cttab()\`,
\`as.data.frame.cttab()\`, and \`write_table()\`. A formatted cttab is
either:

- a \`matrix\` carrying a \`row_style\` attribute, or

- a \`data.frame\` with a \`label\` column (the layout produced by
  \`cttab_format()\` for new long-format inputs).

A plain \`data.frame\` carrying the \`cttab\` class but neither shape
(the user-built legacy table) is \*not\* formatted; callers route those
through the regular data.frame renderer.

## Usage

``` r
is_formatted_cttab(x)
```

## Arguments

- x:

  A \`cttab\` object (or anything else; the check is permissive).

## Value

\`TRUE\` if \`x\` already has a renderer-ready shape, \`FALSE\`
otherwise.
