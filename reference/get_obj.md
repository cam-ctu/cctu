# Gets copies of objects from parent environments, or returns alternative if not found

Gets copies of objects from parent environments, or returns alternative
if not found

## Usage

``` r
get_obj(name, frame = parent.frame(), alt = NULL)
```

## Arguments

- name:

  text string of an object to find

- alt:

  an alternative to return if the object is not found

## Value

either a copy of the object `name` or an alternative if it does not
exist

## Details

this is designed to use within functions in the library to access global
objects, but provide a route to avoid hard-coding specific variable
names in. It is not exported.

## Examples

``` r
library(cctu)
rm(PATH)
#> Warning: object 'PATH' not found
cctu:::get_obj("PATH")
#> Warning: PATH not found
#> NULL
cctu:::get_obj("PATH", alt = getwd())
#> Warning: PATH not found
#> [1] "/home/runner/work/cctu/cctu/docs/reference"
PATH <- "C:/MyFile"
cctu:::get_obj("PATH")
#> [1] "C:/MyFile"
cctu:::get_obj("PATH", alt = getwd())
#> [1] "C:/MyFile"
```
