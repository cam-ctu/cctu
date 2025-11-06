# Function to save ggplot figures.

This is a wrapup function of
[`write_plot`](https://cam-ctu.github.io/cctu/reference/write_plot.md)
to print `ggplot` object.

## Usage

``` r
write_ggplot(
  plot = last_plot(),
  number = cctu_env$number,
  width = 29.7 * 0.6,
  height = 21 * 0.6,
  dpi = 300,
  units = "cm",
  clean_up = TRUE,
  directory = file.path(getOption("cctu_output", default = "Output"), "Figures"),
  format = getOption("cctu_fig_format", default = c("png", "eps")),
  graphics_args = NULL,
  verbose = options()$verbose,
  footnote = NULL
)
```

## Arguments

- plot:

  the plot object to save. defaults to `last_plot`

- number:

  the number used to as a suffix in the output filename, and to link to
  TableofTables. Default is to use the value in the cctu_env package
  environment that is set within
  [`attach_pop`](https://cam-ctu.github.io/cctu/reference/attach_pop.md).

- width:

  the width to save as

- height:

  the height to save as

- dpi:

  the resolution setting

- units:

  either "cm" (the default) or "inches"

- clean_up:

  logical to invoke the
  [`clean_up`](https://cam-ctu.github.io/cctu/reference/clean_up.md)
  function at the end. Defaults to TRUE

- directory:

  where to save the figures within path or current working directory.
  The Output directory can be over-riden with options("cctu_output").

- format:

  either `"png"`, `"eps"`, `"ps"`, `"pdf"`, `"svg"`, `"jpg"` or `"jpeg"`
  to determine the file type to use. You can request multiple file types
  by providing a vector. The `"png"` format will always be produced and
  saved under `directory`, typically this is `Output/Figures`. All other
  formats will be saved under a sub-folder with the format name. See
  details for more information. You can set the default figure format by
  setting `options(cctu_fig_format = c("png", "eps"))` (default).

- graphics_args:

  a list of named arguments to supply to graphics function.
  [png](https://rdrr.io/r/grDevices/png.html) for `"png"`,
  [postscript](https://rdrr.io/r/grDevices/postscript.html) for `"ps"`
  or `"eps"`, [jpeg](https://rdrr.io/r/grDevices/png.html) for `"jpg"`
  and `"jpeg"`, [pdf](https://rdrr.io/r/grDevices/pdf.html) for
  `"pdf"`). If you have the `ragg` package installed, it will use
  [agg_png](https://ragg.r-lib.org/reference/agg_png.html) for `"png"`
  and [agg_jpeg](https://ragg.r-lib.org/reference/agg_jpeg.html) for
  `"jpg"` and `"jpeg"` for better quality. The
  [svglite](https://svglite.r-lib.org/reference/svglite.html) is used
  for `"svg"` file.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

- footnote:

  character vector, can be used to add footnotes.

## Value

writes a copy of a plot to file fig_number. edits the TableofTables
object with the calling program No return object.

## See also

[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
[`write_plot`](https://cam-ctu.github.io/cctu/reference/write_plot.md)
