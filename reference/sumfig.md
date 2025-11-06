# Produce ggplot to represent summary statistics graphically, either boxplot or barchart

Produce ggplot to represent summary statistics graphically, either
boxplot or barchart

## Usage

``` r
sumfig(variable, arm, label = NULL, data = parent.frame(), ...)
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

- ...:

  list of other values to input into the aes() function

## Value

a ggplot object, either a boxplot or barchart
