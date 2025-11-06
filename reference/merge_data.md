# Merge vertically split data

This function can be used when the MACRO dataset is **vertically**
split, and must not be used to combine horizontally split data which has
same variable names. It will rename the variable of the dataset,
including shortcode of dlu and clu files with underscore and number, and
combine them to single dataset. Returns a list with merged dataset, DLU
and CLU. Data, DLU and CLU should be in same order. The merged dataset,
dlu and clu can further be passed to
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md).

## Usage

``` r
merge_data(datalist, dlulist, clulist)
```

## Arguments

- datalist:

  A list of dataset

- dlulist:

  A list of DLU data

- clulist:

  A list of CLU data

## Value

A list of three data.frame

- data: Merged dataset.

- dlu: Merged DLU data.

- clu: Merged CLU data.

## See also

[`read_data`](https://cam-ctu.github.io/cctu/reference/read_data.md)
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)

## Examples

``` r
# Read example data
dt_a <- read.csv(system.file("extdata", "test_A.csv", package="cctu"),
                 colClasses = "character")
dt_b <- read.csv(system.file("extdata", "test_B.csv", package="cctu"),
                 colClasses = "character")

# Read DLU and CLU
dlu_a <- read.csv(system.file("extdata", "test_A_DLU.csv", package="cctu"))
dlu_b <- read.csv(system.file("extdata", "test_B_DLU.csv", package="cctu"))
clu_a <- read.csv(system.file("extdata", "test_A_CLU.csv", package="cctu"))
clu_b <- read.csv(system.file("extdata", "test_B_CLU.csv", package="cctu"))

# Merge dataset with merge_data function
res <- merge_data(datalist = list(dt_a, dt_b), dlulist = list(dlu_a, dlu_b),
                  clulist = list(clu_a, clu_b))
dt <- res$data # Extract combined data
dlu <- res$dlu # Extract combined DLU data
clu <- res$clu # Extract combined CLU data

# Apply CLU and DLU files
dt <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)
```
