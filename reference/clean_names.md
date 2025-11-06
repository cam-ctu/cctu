# Tidies up names of a data frame within certain rules

Tidies up names of a data frame within certain rules

## Usage

``` r
clean_names(df, convert = TRUE, verbose = options()$verbose)

clean_string(x)
```

## Arguments

- df:

  a data frame

- convert:

  a logical to indicate if you want to modify `df` in the parent
  environment, or if not simply return a character vector of the revised
  variable names.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- x:

  a character string to be cleaned.

## Value

`df` is modified to have variable names that are

- lower case

- dots, slashes,+ replaced with \_

- no leading or trailing whitespace

so a variable called "Nasty/Var+Name " gets turned into "nasty_var_name"
.

`clean_names` returns either a vector of cleaned names is return
directly, or the input object is modified in the parent envirnoment (the
default) and an invisible copy of the modified input returned

`clean_string` returns an individual vector with the contents cleaned.
