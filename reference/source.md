# A modified version of the source() function to capture in a data.frame what file was called and from which file

A modified version of the source() function to capture in a data.frame
what file was called and from which file

## Usage

``` r
source(
  file,
  local = getOption("cctu_source_local", default = FALSE),
  backup = NULL,
  ...
)
```

## Arguments

- file:

  a [connection](https://rdrr.io/r/base/connections.html) or a character
  string giving the pathname of the file or URL to read from. The
  [`stdin()`](https://rdrr.io/r/base/showConnections.html) connection
  reads from the console when interactive.

- local:

  `TRUE`, `FALSE` or an environment, determining where the parsed
  expressions are evaluated. `FALSE` (the default) corresponds to the
  user's workspace (the global environment) and `TRUE` to the
  environment from which `source` is called.

- backup:

  A character string giving the file path to which to save an image of
  the R environment before sourcing any code. Default is NULL, which
  does not save any image.

- ...:

  other parameters to be passed to
  [`source`](https://rdrr.io/r/base/source.html).

## Value

no return value

## Details

evaluates code in the file argument, and also augments or creates a
dataframe with two variables (parent, child). The intention is to record
the architecture of a sequenece of code files run using nested source()
statements. To override this function use `source <- base::source`.

## See also

[`source`](https://rdrr.io/r/base/source.html)
