# A function to remove all attached objects from the search path with any exceptions specified

A function to remove all attached objects from the search path with any
exceptions specified

## Usage

``` r
rm_envir(
  ignore = c(".GlobalEnv", "package:", "tools:", "Autoloads"),
  verbose = options()$verbose,
  perl = FALSE
)
```

## Arguments

- ignore:

  a character vector of regular expression search terms within the
  output from [`search()`](https://rdrr.io/r/base/search.html) as to
  which should be retained.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- perl:

  logical. Should Perl-compatible regexps be used?

## Details

it detaches anything not ignored. It is called by default within
[`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md) .

## See also

[`search`](https://rdrr.io/r/base/search.html)
[`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md)
