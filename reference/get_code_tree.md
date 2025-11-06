# track which files of code are called by which oher files

track which files of code are called by which oher files

## Usage

``` r
get_code_tree()

reset_code_tree(root_file = "main.R")
```

## Arguments

- root_file:

  the name of the code file you want to use as the initial root for code
  tree

## Value

the data file containing two variables: parent ; child.

## Details

each time the `cctu` function
[`source`](https://cam-ctu.github.io/cctu/reference/source.md) is
called, an internal data frame is appended with the parent file that
called [`source`](https://cam-ctu.github.io/cctu/reference/source.md),
and the child file that was sourced.

You can reset the code_tree or examine it, but not directly edit it. It
is reset automatically if
[`cctu_initialise`](https://cam-ctu.github.io/cctu/reference/cctu_initialise.md)
is called.

## Functions

- `reset_code_tree()`: reset the internal code_tree object to have no
  entries

## See also

[`cctu_initialise`](https://cam-ctu.github.io/cctu/reference/cctu_initialise.md)
