# Convert a long-format cttab to a rendering-ready table.

Reshapes the long-format object from
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md) into a wide
character data.frame and stamps a `row_style` attribute (one string per
row) for `print.cttab` /
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
to consume. A `label` column holds the display label for each row
(formerly the matrix row-names).

## Usage

``` r
cttab_format(x)
```

## Arguments

- x:

  A `cttab` object returned by
  [`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md).

## Value

A `data.frame` with a `label` column, one column per group level (or
`Total`), a `row_style` attribute, and class `c("cttab", "data.frame")`.

## Details

Section banners (the `row_split` value, the named-list label) and
variable-label headers are inserted automatically. `row_style` is a
per-row character vector whose entries are `;`-joined tokens drawn from
`{"bold", "bgcol", "span", "indent"}`. `""` marks a plain stat row.
Common combinations:

|                     |                                               |
|---------------------|-----------------------------------------------|
| `"bold;bgcol;span"` | section banner (grey background, spanned).    |
| `"bold;span"`       | variable-label / sub-section header.          |
| `"bold"`            | single bold data row (Observation / logical). |
| `"indent"`          | indented stat row.                            |
| `""`                | plain stat row.                               |

Empty-stat-row drop is performed upstream by
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md), so this
function only does layout.

The `nest` attribute on `x` controls the row hierarchy: `"split"` =
row-split outer / variable inner; `"var"` flips it. The actual layout is
delegated to
[`group_data`](https://cam-ctu.github.io/cctu/reference/group_data.md).

## See also

[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md),
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md),
[`group_data`](https://cam-ctu.github.io/cctu/reference/group_data.md)
