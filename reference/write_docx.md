# Produces the final Docx file

Produces the final Docx file

## Usage

``` r
write_docx(
  report_title,
  author,
  meta_table = get_meta_table(),
  filename = file.path(cctu_opt("output"), "Reports", "Report.docx"),
  table_path = file.path(cctu_opt("output"), "Core"),
  figure_format = "png",
  figure_path = file.path(cctu_opt("output"), "Figures"),
  popn_labels = NULL,
  verbose = options()$verbose,
  keep_xml = FALSE
)
```

## Arguments

- report_title:

  text string used to label the report title page

- author:

  text string naming the author

- meta_table:

  a data frame that contains meta information on tables (title,
  population, number). Defaults to
  [`get_meta_table()`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md).

- filename:

  text string giving the filename/path to output the word document to.

- table_path:

  text string giving the filepath to the tables folder. This is used to
  directly open table files and copy the content.

- figure_format:

  it only supports `png` format.

- figure_path:

  text string giving the filepath to the figures folder.

- popn_labels:

  alternative text string giving labels used for the population - might
  want to include the population size. They must match
  `unique(meta_table$population)`, excluding rows with a blank
  population.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- keep_xml:

  a boolean if the compiled XML should be kept, used for debugging
  purposes.

## Value

This function is run for its side-effects: creates an xml document that
glues together all the outputs and meta data as per the meta-table
argument; a transformation fo this as per the xslt file, the default can
be opened as a word document.

## Details

suggest that [`file.path`](https://rdrr.io/r/base/file.path.html) is
used to create non default file paths, to cope with OS vaguaries.
