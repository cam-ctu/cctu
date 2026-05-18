# Evaluate a \`select\` filter expression for one variable.

\`select\` is a named character vector / list whose names are variable
names and whose values are R-expression strings evaluated against
\`data\`. The contract matches \`stat_tab()\`: rows where the expression
is \`FALSE\` or \`NA\` are excluded.

## Usage

``` r
cttab_eval_select(data, var, select)
```

## Arguments

- data:

  A \`data.frame\` / \`data.table\` providing the evaluation
  environment.

- var:

  Name of the variable whose filter should be evaluated.

- select:

  Named filter list (or \`NULL\`).

## Value

Logical vector of length \`nrow(data)\`. Returns all-\`TRUE\` when
\`select\` is \`NULL\` or when \`var\` has no entry in \`select\`. On
evaluation error a warning is emitted and all-\`TRUE\` is returned.
