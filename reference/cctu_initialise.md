# initialise objects for using cctu package

initialise objects for using cctu package

## Usage

``` r
cctu_initialise(
  root = getwd(),
  scripts = FALSE,
  rm = FALSE,
  description = TRUE,
  output = getOption("cctu_output", default = "Output")
)

cctu_initialize(
  root = getwd(),
  scripts = FALSE,
  rm = FALSE,
  description = TRUE,
  output = getOption("cctu_output", default = "Output")
)

cctu_check_dir(
  root = getwd(),
  warnings = FALSE,
  output = getOption("cctu_output", default = "Output")
)

rm_output(
  output = getOption("cctu_output", default = "Output"),
  core = TRUE,
  figures = TRUE,
  reports = TRUE,
  top = TRUE
)
```

## Arguments

- root:

  the root directory to start in

- scripts:

  logical create the standard set of scripts. Intended to be used once
  interactively at the start of coding for an analysis.

- rm:

  logical whether to also run `rm_output` with its default values, to
  delete all the files in the output directory.

- description:

  logical, whether to create a DESCRIPTION file if one does not exist.

- output:

  character string giving the name of the output folder. Can be
  overriden by setting the option("cctu_output").

- warnings:

  logical indicator to issue warning if the directories do not exist.
  Default FALSE.

- core:

  logical delete the files in /Core

- figures:

  logical delete the files /Figures

- reports:

  logical delete the files in /Reports

- top:

  logical delete top level files that are not in core/figures/reports.

## Value

cctu_initialise gives an invisible return of logical indicating if the
directories have been created. The directories needed are "Output", and
within "Output", "Core", "Figures", "Reports". It also runs
`reset_code_tree(root)` automatically.

cctu_check_dir gives a logical indicating if the directories exist,

## Functions

- `cctu_initialise()`: create the standard directories for outputs if
  needed.

- `cctu_initialize()`: identifical function with American spelling

- `cctu_check_dir()`: Check if the directories exist for cctu

- `rm_output()`: function to clear previous output files, but leaves the
  directories in place

## See also

[`dir.create`](https://rdrr.io/r/base/files2.html)
[`reset_code_tree`](https://cam-ctu.github.io/cctu/reference/get_code_tree.md)
