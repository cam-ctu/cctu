# Function to remove blank rows/columns from a df

Function to remove blank rows/columns from a df

## Usage

``` r
remove_blank_rows_cols(
  df,
  convert = TRUE,
  verbose = options()$verbose,
  which = c("both", "rows", "cols")
)
```

## Arguments

- df:

  a data.frame

- convert:

  a logical to indicate if you want to modify `df` in the parent
  environment, or if not simply return a modified version of `df`

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- which:

  one of `"rows"`, `"cols"`, or `"both"`. Default is `"both"` to
  removing both empty rows and empty columns.

## Value

this removes columns and rows that are totally empty (either "" or NA).
