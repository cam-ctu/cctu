# Function to save plot figures

One may not always use `ggplot2` to draw plot, base `plot` function for
example, this function is particularly useful in that situation.

## Usage

``` r
write_plot(
  ...,
  plot_fn = plot,
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
  footnote = NULL,
  plot_args = NULL
)
```

## Arguments

- ...:

  Arguments to be passed to the ploting function `plot_fn` below.

- plot_fn:

  function to draw a plot. This can be simple `print` OR `plot`
  (default) OR a custom plot drawing function depending on how the plot
  was drawn. See the `details` and `examples` below.

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

- plot_args:

  Deprecated, kept here for backcompatibility. A named list arguments
  for `plot_fn`.

- plot:

  the plot object to save. defaults to `last_plot`

## Value

writes a copy of a plot to file fig_number. edits the TableofTables
object with the calling program No return object.

## Details

\## Saving a plot

The `plot_fn` can be a user defined function to draw plot. Let us assume
the object `p` is your plot object. If you can see the plot by simply
typing `p` on the console, then this should be `print`. If you can see
the plot by simply typing `plot(p)` on the console, then this should be
`plot`.

All parameters should be passed with names. Checkout the examples below.

\## Figure formats

You can request for multiple figure formats when saving a plot. You can
save `"png"` (default), `"eps"`, `"ps"`, `"pdf"`, `"svg"`, `"jpg"`
and/or `"jpeg"` formats. The `"png"` format will always be produced to
produce final Word report. The `"eps"`, `"ps"`, `"pdf"` and `"svg"` are
vector figures and you can post edit figures with tools like
[Inkscape](https://inkscape.org/). The `"svg"` is recommended if you
want to modify the plots. You can then easily export `"svg"` to other
formats, including `"eps"` and `"ps"`, `"pdf"`. The
[svglite](https://svglite.r-lib.org/reference/svglite.html) is used to
generate `"svg"` file.

## See also

[`write_table`](https://cam-ctu.github.io/cctu/reference/write_table.md)
[`write_ggplot`](https://cam-ctu.github.io/cctu/reference/write_ggplot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#####################################
# Below is a simple example ========#
#####################################
#
write_plot(plot_fn = plot, plot_args = list(x = iris[, 1], y = iris[, 2]))
# This is equivalent drawing the following plot and save it
plot(x = iris[, 1], y = iris[, 2])

# Below is user defined function plotting
# One can use this method to draw a complicated plot
new_plot <- function(x, y, h, v) {
  par(pty = "s", cex = 0.7) # adjust plot style
  plot(x, y)
  abline(h = h, v = v, lty = 2) # add some lines
}
write_plot(
  x = iris[, 1], y = iris[, 2], h = 2.5, v = 6.0,
  plot_fn = new_plot
)


####################################################
# To draw a KM-plot from survminer package ========#
####################################################

library("survival")
library("survminer")
fit <- survfit(Surv(time, status) ~ sex, data = lung)
# Drawing survival curves
p <- ggsurvplot(fit, data = lung)
write_plot(p, plot_fn = survminer:::print.ggsurvplot)
# The code above works because the p is a ggsurvplot object (check it with class(p))
# There's a printing function print.ggsurvplot to handle the printing of the KM-plot.
# But this function is not exported by survminer, so we need to use three colons.

#####################################
# Draw a consort diagram ===========#
#####################################

library(grid)
# Might want to change some settings
txt0 <- c("Study 1 (n=160)", "Study 2 (n=140)")
txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=15)"

# supports pipeline operator
g <- add_box(txt = txt0) |>
  add_box(txt = txt1) |>
  add_side_box(txt = txt1_side) |>
  add_box(txt = "Randomized (n=200)")
# Since you can draw the plot g with plot(g), the ploting function is plot
# The plotting function is \code{plot.consort}, so simple plot or plot.consort works
write_plot(g, plot_fn = plot)
# Or just
write_plot(g)
} # }
```
