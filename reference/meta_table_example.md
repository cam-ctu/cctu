# Example of a meta_table data frame

Example of a meta_table data frame

## Usage

``` r
meta_table_example
```

## Format

a dataset with 10 columns and 3 example rows

- section:

  Section label to help subdivide a report

- title:

  title of each individual item in a repoert

- subtitle:

  optional subtitle values. Use the empty string if no subtitle wanted.

- number:

  the tem number, to use for link up with
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md),
  [`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md),
  [`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md).

- population:

  the name of the population to use for the item. See
  [`create_popn_envir`](https://cam-ctu.github.io/cctu/reference/create_popn_envir.md).

- orientation:

  takes values "portrait" or "landscape" to determine the page
  orientation for each item

- margin:

  takes values "normal" or "narrow" to determine the page margin for
  each item

- item:

  takes values "table", "figure", or far more rarely "text".
  [`write_docx`](https://cam-ctu.github.io/cctu/reference/write_docx.md)
  will expect there to exist 'table_XX.xml', 'figure_XX.png' or
  'text_XX.xml' as appropriate where 'XX' is the number value.

- footnote1:

  optional footnote. Use the empty string if no subtitle wanted.

- footnote2:

  optional footnote. Use the empty string if no subtitle wanted.

- fontsize:

  optional fontsize. Set the font size used in a table in units 1/144 of
  inch. Defaults to 20 if not set.

## Details

This is an example of the structure of a meta_table that must exist
internally to use
[`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md),
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md),
[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md),
[`write_docx`](https://cam-ctu.github.io/cctu/reference/write_docx.md).
All variables are characters, including 'number'. The variables
'orientation' and 'item' can only take specific values.

Typically one would create raw data using a spreadsheet editor and read
it into R. You may add additional columns to help plan and understand
what each table or figure will be, and track progress.

See `system.file("extdata", "meta_table.xlsx", package = "cctu")`.

Then you must set it internally using
[`set_meta_table`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md),
and possibly examine the contents with
[`get_meta_table`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md).

To preserve troublesome number values '1.10', or '1.1.1', in Excel one
can prefix the ' character (to the left of the \# key) before the number
value.

## See also

[`set_meta_table`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md)
[`get_meta_table`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md)
