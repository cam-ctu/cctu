# Produces the final xml file

Produces the final xml file

## Usage

``` r
create_word_xml(
  report_title,
  author,
  meta_table = get_meta_table(),
  datestamp = format(Sys.time(), format = "%H:%M %d %b %Y"),
  filename = file.path(getOption("cctu_output", default = "Output"), "Reports",
    "Report.doc"),
  table_path = file.path(getOption("cctu_output", default = "Output"), "Core"),
  figure_format = c("png", "jpeg", "ps"),
  figure_path = file.path(getOption("cctu_output", default = "Output"), "Figures"),
  popn_labels = NULL,
  verbose = options()$verbose,
  xslt_file = system.file("extdata", "to_word.xslt", package = "cctu"),
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
  population, number). Defaults is get_meta_table()

- datestamp:

  text used to give the date-time stamp, defaults to the system
  date/time at the time of running the function

- filename:

  text string giving the filename/path to output the word document to

- table_path:

  text string giving the filepath to the tables folder. This is used to
  directly open table files and copy the content.

- figure_format:

  the format to look for figure files when building the report ("png",
  "jpeg","ps")

- figure_path:

  text string giving the filepath to the figures folder. This is used to
  create a reference within the word document. Hence it needs to be a
  relative (to the output report) path "../Figures". Likely to lead to
  empty figures in the report if it is changed.

- popn_labels:

  alternative text string giving labels used for the population - might
  want to include the population size... They must match correctly to
  unique(meta_table\$population), excluding rows with a blank, or no,
  population given

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- xslt_file:

  a text file containing the xslt document. Default is
  system.file("extdata", "xml_to_word.xslt", package="cctu").

- keep_xml:

  a boolean if the compiled XML should be kept, used for debugging
  purpose.

## Value

This function is run for its side-effects: creates an xml document that
glues together all the outputs and meta data as per the meta-table
argument; a transformation fo this as per the xslt file, the default can
be opened as a word document.

## Details

suggest that [`file.path`](https://rdrr.io/r/base/file.path.html) is
used to create non default file paths, to cope with OS vaguaries.
