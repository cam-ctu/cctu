# Functions that attach and detach environments based on which populations are used for a given table number

Functions that attach and detach environments based on which populations
are used for a given table number

## Usage

``` r
attach_pop(number, verbose = options()$verbose, rm_envir = TRUE)

detach_pop(number, verbose = options()$verbose)
```

## Arguments

- number:

  a character string, or number, giving the number of a table or figure.
  Be careful if you want '2.10' rather than 2.1, say .

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- rm_envir:

  logical, default=TRUE. Whether to run
  [`rm_envir`](https://cam-ctu.github.io/cctu/reference/rm_envir.md)
  first before attaching a population.

## Value

invisibly returns an environment for attaching, or NULL for detaching.

## Functions

- `attach_pop()`: attaches a population

- `detach_pop()`: detaches a populations

## See also

[`attach`](https://rdrr.io/r/base/attach.html)
[`detach`](https://rdrr.io/r/base/detach.html)
[`rm_envir`](https://cam-ctu.github.io/cctu/reference/rm_envir.md)
