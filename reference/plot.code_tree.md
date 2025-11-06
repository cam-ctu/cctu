# plot the code tree.

plot the code tree.

## Usage

``` r
# S3 method for class 'code_tree'
plot(x, root_file = "main.R", ...)
```

## Arguments

- x:

  code tree object from
  [`get_code_tree`](https://cam-ctu.github.io/cctu/reference/get_code_tree.md).

- root_file:

  the name of the code file you want to use as the initial root for code
  tree

- ...:

  other parameters to be passed to
  [`plot.igraph`](https://r.igraph.org/reference/plot.igraph.html)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(get_code_tree(), root_file = "main.R")
} # }
```
