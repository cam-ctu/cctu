# Changelog

## cctu 0.8.9

- Bug fix to cttab when `group` is the variable name in the data set
  that you want to use as the `group` argument.

- Passing lint tests. Argument to
  [`to_factor()`](https://cam-ctu.github.io/cctu/reference/to_factor.md)
  “drop.levels” is now “drop_levels”. Other internal changes to
  indentation and variable naming .

## cctu 0.8.8

- write_plot() and write_ggplot() (retired function), now accept
  multiple values to the format argument. The default is to create both
  the PNG format for use in a docx report, and also eps, saved in a
  subfolder, to have a format that is accepted by most journals.

- minor bugfixes

- improved unit testing locally and for CI on GitHub.

## cctu 0.8.7

- Tools with `cctu_initialise` and `library_description` to record and
  load packages using a DESCRIPTION file.

- Better code_tree plotting.

- Fixes to `cttab` when a variable is totally missing.

- Updates to vignettes to give pointers and advice on using Quarto.

## cctu 0.8.6

- Applied testing and edits with `lintr` and `styler` to meet tidyverse
  style with the r code.

- Fixed issue with
  [`source()`](https://cam-ctu.github.io/cctu/reference/source.md) so it
  now should work with the rstudio button.

- Partially set up pkdown site version of the docs- but not currently
  able to include the vignettes.

## cctu 0.8.4

- Added in km_ggplot() function to produce publication-quality
  Kaplan-Meier figures with error bands and table underneath.

- Added in options(“cctu_p_digits”) with default of 4 for
  regression_table() and km_ggplot()

## cctu 0.8.3

- Fixed a bug with write_docx() in the multi-line headers are now
  completely visible. Testing added of docx outputs.

- Added in options(cctu_output) and optinos(cctu_source_local) to change
  the default arguments for write_xx() and source()

- Updated Vignette

## cctu 0.8.2

- Added write_docx() which creates directly a fully compliant OfficeOpen
  docx file, no subsequent steps needed, and it does open on the online
  office/word tools.

## cctu 0.8.1

- Fixing the GitHub continuous integration. Minor fixes to rbind_space,
  data_table_summary

## cctu 0.8.0

- Added in the regression_table() generic to print a nice tidy table to
  present regression models. Minor improvements and updates to other
  functions including cttab and write_plot.

## cctu 0.7.6

- The apply_macro_dict() function is faster now. By default, evaluating
  whether a category variable’s type is numeric before converting is
  skipped.
- Table and figure numbers are locked, saving Word to PDF will not
  change the numbers.
- Figures are embedded in the document. No need to perform “save picture
  in document”.
- Dynamically add footnotes to the tables and figures in write_table()
  and write_ggplot().
- There’s a new function write_plot() to save figures other than the
  ggplot family, KM-plot from survminer for example.
- Bugfix: headings will be used if it is provided in the write_table().
- p_format will round pvalues and convert to \<0.001 as a character
  variable.
