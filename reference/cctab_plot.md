# Create summary plot for cctab function

Create summary plot for cctab function

## Usage

``` r
cctab_plot(vars, data, group = NULL, row_split = NULL, select = NULL)
```

## Arguments

- data:

  A `data.frame` from which the variables in `vars` should be taken.

- group:

  Name of the grouping variable.

- row_split:

  Variable that used for splitting table rows, rows will be split using
  this variable. Useful for repeated measures.

- select:

  a named vector with as many components as row-variables. Every element
  of \`select\` will be used to select the individuals to be analyzed
  for every row-variable. Name of the vector corresponds to the row
  variable, element is the selection.
