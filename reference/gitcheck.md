# Checks for problem files to avoid saving in github

Checks for problem files to avoid saving in github

## Usage

``` r
gitcheck(
  folder = ".",
  extensions = c("csv", "xls", "xlsx", "png", "eps", "jpg", "pdf", "doc", "docx", "zip")
)
```

## Arguments

- folder:

  The folder to run checks within. Needs to be a git repository.
  Defaults to current working directory

- extensions:

  A character vector giving the file extensions to search for

## Value

invisibly returns a list of the un-tracked and tracked problem files.
Will print on terminal a report as the main objective.
