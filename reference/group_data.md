# Create Nested Group Header Rows in a data.table

Transforms a `data.table` by inserting header rows for each level of the
specified grouping variables. The grouping variables are moved to the
front of the dataset.

## Usage

``` r
group_data(data, groups, shift_to = NULL, indent = FALSE, carry = NULL)
```

## Arguments

- data:

  A `data.table`. If a `data.frame` is provided, it will be converted.

- groups:

  A character vector of column names to group by (ordered high to low).

- shift_to:

  Character. The name of the column into which group labels are shifted.
  When `NULL` (default) no shifting is performed.

- indent:

  Logical. If `TRUE`, indents nested headers and data rows.

- carry:

  Character vector of column names whose values should be carried into
  header rows rather than set to `NA`. Each named column must have a
  unique value within its group at every nesting level - an error is
  thrown if that is not the case.

## Value

A `data.table` with nested header rows.

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
dt <- data.table(Region = c("North", "North", "South"),
                 Dept = c("Sales", "Sales", "HR"),
                 Staff = c("A", "B", "C"),
                 Sales = c(10, 20, 30))

# Standard headers
group_data(dt, groups = "Region")
#>    Region   Dept  Staff Sales
#>    <char> <char> <char> <num>
#> 1:  North   <NA>   <NA>    NA
#> 2:  North  Sales      A    10
#> 3:  North  Sales      B    20
#> 4:  South   <NA>   <NA>    NA
#> 5:  South     HR      C    30

# Shifted and indented headers for reporting
group_data(dt, groups = "Region", shift_to = "Staff", indent = TRUE)
#>      Dept  Staff Sales
#>    <char> <char> <num>
#> 1:   <NA>  North    NA
#> 2:  Sales      A    10
#> 3:  Sales      B    20
#> 4:   <NA>  South    NA
#> 5:     HR      C    30
# Shifted and indented headers for reporting with multiple grouping levels
group_data(dt, groups = c("Region", "Dept"), shift_to = "Staff", indent = TRUE)
#>       Staff Sales
#>      <char> <num>
#> 1:    North    NA
#> 2:    Sales    NA
#> 3:        A    10
#> 4:        B    20
#> 5:    South    NA
#> 6:       HR    NA
#> 7:        C    30
```
