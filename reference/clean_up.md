# removes all the objects in the environment, apart from those listed in reserved and detaches the environment used to create tables or figures corresponding to the number

removes all the objects in the environment, apart from those listed in
reserved and detaches the environment used to create tables or figures
corresponding to the number

## Usage

``` r
clean_up(
  number,
  frame = parent.frame(),
  reserved_string = ".reserved",
  verbose = options()$verbose
)
```

## Arguments

- number:

  a number associated with a table of figure

- frame:

  an environment in which to clean up

- reserved_string:

  a character giving the name of a global variable that names gloabl
  objects that should generally be preserved globally when tidying up.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

## Value

NULL, but removes objects
