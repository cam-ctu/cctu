# checks to see if a .gitignore needs to be create or edited based on the template file

checks to see if a .gitignore needs to be create or edited based on the
template file

## Usage

``` r
gitignore_init(
  template = system.file("extdata/gitignore_template", package = "cctu")
)
```

## Arguments

- template:

  the template .gitignore file. Default is part of the package data.

## Value

nothing, called for its side effect. Gives messages out if file is
created or edited
