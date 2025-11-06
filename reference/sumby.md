# Produces summary statistics on a variable broken down by arm, display a graphical equivalent.

Produces summary statistics on a variable broken down by arm, display a
graphical equivalent.

## Usage

``` r
sumby(
  variable,
  arm,
  label = NULL,
  data = parent.frame(),
  total = TRUE,
  fig = TRUE,
  directory = file.path(getOption("cctu_output", default = "Output"), "Figures"),
  verbose = options()$verbose,
  text_clean = propercase,
  pct_digits = 0,
  delete = NULL
)
```

## Arguments

- variable:

  A variable of interest to be summarised. Works for several classes:
  numeric, integer, factor, character.

- arm:

  A variable to break down the summary by, typically an arm in a RCT. Or
  a constant variable for 1-armed study

- label:

  a text string to label the variable by, used to add in units or full
  words with spaces rather than acronyms. Defaults to NULL.

- data:

  optional a data frame, or envirnoment, in which to find the variable
  and arm values. Defaults to parent.frame()

- total:

  logical value to include an extra overall columns. Defaults to true

- fig:

  logical value on whether to print (if interactive) and save a copy of
  the figure. Defaults to true

- directory:

  the path to the directory where figures will be saved as
  "sumby_XX_Y.png". XX is taken from the current table numer (or "0")
  set in
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md),
  and the Y is counting how many time `sumby` has been run since the XX
  was last set.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- text_clean:

  a function to transform character labels. Defaults to propercase. Or
  set to NULL if you want to preserve the original text.

- pct_digits:

  number of decimal places to present percentages to. Defaults to 0.

- delete:

  levels of the input variable that should be removed from the output
  table, in the case of frequency tables. Can be useful for a binary
  variable if you just want the rate of "yes" for example.

## Value

a data.frame containing summary statistics in character format, ready to
use with write_table(). Plus an attribute "fig" that contains a ggplot
object

## See also

[`sumfig`](https://cam-ctu.github.io/cctu/reference/sumfig.md) ,
[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md),
[`propercase`](https://cam-ctu.github.io/cctu/reference/propercase.md),
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
