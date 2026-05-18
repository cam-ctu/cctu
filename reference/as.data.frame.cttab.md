# Coerce a cttab to a tidy data frame.

Returns a clean long-format `data.frame` with one row per (row_split,
group, variable, statistic) cell, dropping the rendering meta-columns
(`Group_ID`, `Group_Label`, `Var_ID`, `Stat_ID`). Useful for piping
`cttab` output into other table tools.

## Usage

``` r
# S3 method for class 'cttab'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `cttab` object.

- row.names, optional, ...:

  Inherited from
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A `data.frame`.

## Details

For an already-formatted matrix `cttab`, the standard matrix coercion is
used.
