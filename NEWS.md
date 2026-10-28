# cctu 0.8.16

Correctness review of `cttab()` and the statistics it renders. Each of the bugs below could silently put a wrong or missing number into a report; all now have regression tests.


* `select` filters that reference the `group` or `row_split` variable are now an error. Such a filter previously left the group columns unfiltered while still filtering the Total column, so Total reported a single group rather than the whole population. Grouping variables can only be used as `group` or `row_split`.

* `rbind()` of `cttab` objects with different `row_split` variables is now an error. It previously replaced every value in the combined table — including the well-formed part's — with a row count. Stack tables that share a row split, or format them separately.

* A `group` level named `"Total"` is now an error, as it collides with the generated Total column. Rename the level or pass `total = FALSE`.

* `select` filters now evaluate against the original values rather than value-label text. A filter such as `c(BMIBL = "RACEN != 1")` was silently ignored when `RACEN` was also being summarised, because it had been converted to a factor first.

* `select` filters are evaluated against the data columns and base functions only; they no longer see the user's workspace.

* `round_pad()` now rounds negative numbers away from zero. `round_pad(-0.135, 2)` returned `-0.13` instead of `-0.14`, systematically biasing signed values (e.g. change from baseline) toward zero.

* `render_numeric()` no longer substitutes a statistic name inside a longer one that contains it — `"Mean (GMean)"` rendered as `"25.0 (G25.0)"`.

* A statistic that cannot be computed renders `NA` when it sits beside one that could (e.g. `Mean (SD)` for a single observation reads `"5.00 (NA)"`), and blank when nothing in the cell could be computed. Previously the two rendering paths disagreed — `"5.00 (NA)"` from one, `""` from the other — for the same input.

* `CV` is reported as unavailable when the mean is zero, instead of `"Inf%"` or `"NaN"`.

* `Inf` values are no longer silently dropped from rendered statistics.

* A categorical variable that is entirely missing now reports `Missing` instead of disappearing from the table.

* `cttab_plot()` evaluates all `select` filters before applying any, so the plot agrees with the table and no longer depends on the order of the names in `select`.

* `group_data()` no longer returns a `data.table` carrying a stale key describing the input's row order.

* `cttab_format()` refuses to render a table whose rows are not uniquely identified, rather than silently emitting row counts in place of statistics.

* `cttab_plot()` returns its list of plots invisibly.

* Documentation corrections: `cat_stat()` no longer claims zero-count levels are rendered (`render_cat()` blanks them), and `num_stat()` no longer documents `q25`/`q50`/`q75` aliases that do not exist.

* Correcting a bug in `read_data()`  whereby mixing optional arguments for different file types caused an error.

# cctu 0.8.11

Trying to fix links in Package Down.


# cctu 0.8.10

Worked out how to make the vignettes' links to output files actually work. Fixed
`analysis-template` and `rmarkdown_report`. See the `\vignette\readme.txt`



# cctu 0.8.9

* Bug fix to cttab when `group` is the variable name in the data set that you want 
to use as the `group` argument.

* Passing lint tests. Argument to `to_factor()` "drop.levels"  is now "drop_levels".
Other internal changes to indentation and variable naming .

# cctu 0.8.8

* write_plot() and write_ggplot() (retired function),  now accept multiple values to
the format argument.  The default is to create both the PNG format for use in a docx
report, and also eps, saved in a subfolder, to have a format that is accepted by
most journals.

* minor bugfixes

* improved unit testing locally and for CI  on GitHub.

# cctu 0.8.7

* Tools with `cctu_initialise` and `library_description` to record and load packages using a
DESCRIPTION file.

* Better code_tree plotting.

* Fixes to `cttab` when a variable is totally missing.

* Updates to vignettes to give pointers and advice on using Quarto.


# cctu 0.8.6

* Applied testing and edits with `lintr` and `styler` to meet tidyverse style with the r code.

* Fixed issue with `source()` so it now should work with the rstudio button.

* Partially set up pkdown site version of the docs- but not currently able to include the vignettes.

# cctu 0.8.4

* Added in km_ggplot() function to produce publication-quality Kaplan-Meier figures with error bands and table underneath.

* Added in options("cctu_p_digits") with default of 4 for regression_table()  and km_ggplot()

# cctu 0.8.3

* Fixed a bug with write_docx() in the multi-line headers are now completely visible. Testing added of docx outputs.

* Added in options(cctu_output) and optinos(cctu_source_local) to change the default arguments for write_xx() and source()

* Updated Vignette


# cctu 0.8.2

* Added write_docx() which creates directly a fully compliant OfficeOpen docx file,
no subsequent steps needed, and it does open on the online office/word tools.

# cctu 0.8.1 

* Fixing the GitHub continuous integration. Minor fixes to rbind_space, data_table_summary


# cctu 0.8.0

* Added in the regression_table() generic to print a nice tidy table
to present regression models. Minor improvements and updates to other functions including cttab and write_plot.



# cctu 0.7.6

* The apply_macro_dict() function is faster now. By default, evaluating whether a category variable's type is numeric before converting is skipped.
* Table and figure numbers are locked, saving Word to PDF will not change the numbers.
* Figures are embedded in the document. No need to perform “save picture in document”.
* Dynamically add footnotes to the tables and figures in write_table() and write_ggplot().
* There’s a new function write_plot() to save figures other than the ggplot family, KM-plot from survminer for example.
* Bugfix: headings will be used if it is provided in the write_table().
* p_format will round pvalues and convert to <0.001 as a character variable.

