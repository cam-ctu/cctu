# Combine descriptive statistics tables by rows.

Stacks two or more long-format
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md) objects
without formatting them. The inputs must share the same grouping
variable (`attr(., "group")`) so they line up as a single rendered
table; the `nest` mode is taken from the first input. The bound parts
are kept as separate sections in the rendered output by offsetting their
`Group_ID` / `Var_ID` so subsequent variables sort after earlier ones
rather than interleaving.

## Usage

``` r
# S3 method for class 'cttab'
rbind(...)
```

## Arguments

- ...:

  `cttab` objects to stack.

## Value

A `cttab` object of the same flavour (long-format `data.table` or
formatted matrix) as the inputs.

## Details

Already-formatted matrix `cttab` objects (e.g. produced by
[`cttab_format`](https://cam-ctu.github.io/cctu/reference/cttab_format.md))
are stacked by binding their character matrices and concatenating their
`position` attributes. Mixing matrix and long-format inputs is not
supported.
