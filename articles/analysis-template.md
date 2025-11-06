# Analysis Template

## What this Document Does

The package `cctu` is designed to help run the analysis of clinical
trials which typically produce an output of a large word document with
one page per table or figure following on from a Statistical Analysis
Plan, and requires substantial audit trails. Automating the trivialities
of getting tables into word from R, and capturing the meta data and
audit trials is what the package achieves.

This document should give you an overview of the tools provided in this
package, explain the thought behind them, and show how to use them
together. We will start by explaining the rationale, and then finish
with a worked example. A complimentary set of R scripts and outputs,
that aren’t easily directly captured within a vignette are also provided
with the /doc directory.

Further functions and tools have been added, for more streamlined
importing of MACRO data, give improvements to the basic tables, and to
automate the reporting of missing data. These are explained more in the
separate `using-cttab` vignette.

## Key Concepts

### Meta Table

As part of writing the Statistical Analysis Plan (SAP), a structured
meta-table with one row per table or figure needs to be produced. This
provides titles, subtitles, numbering, populations, etc. See the help
page `meta_table` for more details. The package is very reliant on this
being provided by the user. Whilst producing the tables/figures the
package will:

- look up which population is to be used for a table/figure and
  explicitly filter the data sets to the desired population
- whilst saving the output it will use the numbering to determine file
  names of xml or graphics files
- write the name of the code file that produced the output

When producing the final output it will use the meta-table to index all
the outputs, and pull out the meta-data needed.

Given all the interactions needed, which is somewhat contrary to the
concept in R of functions just returning output rather than modifying
the user environment, the meta_table is stored within a environment that
is private to the package. So it needs to be set up using
[`set_meta_table()`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md)
and
[`get_meta_table()`](https://cam-ctu.github.io/cctu/reference/get_meta_table.md)
to extract a copy.

### Populations

It is assumed that there is a data.frame that has one row per subject,
and a column for each population defined in the SAP that is a logical
indicating if a subject is included in each population. The definition
of the populations, their names, and how the inclusion is to determined,
are all important steps but outside of this package!

We provide a function `create_popn_envir` that takes the data.frame
described above, and a list of other data.frames. It then filters each
of the listed data.frame, looping over each of the populations, and
saves them within a set of environments one per population.

During the analysis code, the function
[`attach_pop()`](https://cam-ctu.github.io/cctu/reference/attach_pop.md)
is repeatedly called with the number of a table/figure. The
corresponding row from meta-table is used to identify which population
is associated with the table/figure, and the environment is attached. So
the list of dataframes that were filtered in `create_popn_envir` can now
be accessed, safe in the knowledge that the right population is used.

### Code Architecture

A set of default local files can be set up using `cctu_initialise` where
the outputs will be stored using the default arguments.

The code itself is assumed to be modularised into a sequence of code
that calls other code using `source`.

- `Main.R` at the top with a minimal amount of initialisation: working
  directory, PATHs, [`library(cctu)`](https://cam-ctu.github.io/cctu/),
  [`run_batch()`](https://cam-ctu.github.io/cctu/reference/run_batch.md),
  then a sequence of
  [`source()`](https://cam-ctu.github.io/cctu/reference/source.md),
  final calling of
  [`create_word_xml()`](https://cam-ctu.github.io/cctu/reference/create_word_xml.md)
  and maybe further calls to `render` for other outputs.
- A configuration file, loading packages, setting graphical defaults,
  loading bespoke functions,
- Data import and manipulation: where cleaning, merging, creating
  derived variables maybe, and finally using `create_popn_envir`
  happens.
- Analysis: where the actual statistical work happens. May well be
  modularised further with individual scripts for each section of the
  study, ordered by Form or chronological ordering of the study.

It is important to call
[`library(cctu)`](https://cam-ctu.github.io/cctu/) before the first use
of `source`. The package modifies the base function
[`base::source`](https://rdrr.io/r/base/source.html), so as to record
the name of the file that called source and the name of the file
sourced. This is to provide an audit trail allowing the sequence of code
used to create a table/figure to be easily found. Like `meta_table` this
audit trail lives in a private environment and is not easily directly
accessible.

### XML

The step of transferring table and figures to word is facilitated by
xml. The tables are stored as a copy on a local drive (in /Output/Core
by default) in semi-readable format with tags similar to html tables.

During the analysis, a block of code to produce a table or figure is put
between an
[`attach_pop()`](https://cam-ctu.github.io/cctu/reference/attach_pop.md)
and a
[`write_table()`](https://cam-ctu.github.io/cctu/reference/write_table.md)
or `write_gglot()`. An argument to `write_table` will be a data frame;
`write_ggplot` defaults to using the last plot object, or can take a
ggplot object as an argument. A recent addition is `write_plot` which
can handle ggplot object or other graphical outputs e.g. a dot_plot from
the `eudract` package.

All `write_*` functions then look up the number of the table called by
`attach_pop` (temporarily stored in a private environment); the output
is stored on the local drive either an XML file, or graphics file named
with a suffix “\_X.Y.Z” to identify the output; the name of the code
file that created the output is written to meta-table; by default a
final call to `clean_up` removes all objects in the global environment
(apart from those named in the vector `.reserved`) and detach the
population environment.

After the analysis, the function `write_docx` glues these files all
together along with the meta data into a large xml file. The tables are
directly copied, and the figures have links to their file paths. Then
xslt technology is used to convert the document into the
[schema](http://schemas.microsoft.com/office/word/2003/wordml) used by
MS Word, so it can be opened directly.

Previously the command `create_word_xml` was used, which then needed
subsequent steps to convert to a fully compliant docx with figures.
Included for back compatibility now.

If you want to tweak the style of the word document then you would edit
the [xslt file](https://cam-ctu.github.io/cctu/extdata/xml_to_word.xslt)
totally at your own risk! This is not the case with `write_docx`
anymore. You are not allowed to eidit this file as the Docx file is
complicated.

### Pointers

Other function may be worth looking up in their help files, and
hopefully their names give a hint!

#### Data import & Manipulation

- `read_data`
- `data_table_summary`
- `clean_names`
- `remove_blank_rows_cols`
- Recent additions are:
  - `var_lab`
  - `apply_macro_dict`
  - `extract_form`

The set of external data files to be read in, along with the names of
the corresponding data.frames created should be recorded in one initial
data.frame. This can then be summarised in a table in the report using
`data_table_summary` and also used to succinctly read in all the data
files with `read_data`.

#### Analysis

- [`sumby()`](https://cam-ctu.github.io/cctu/reference/sumby.md) is a
  work-horse to produce standard summary tables, with additional figures
  produced as side-effects
- `cttab` is a recent addition which greatly expands the flexibility and
  scope from `sumby`
- `regression_table` is a function that takes a regression object (lm,
  glm, lme, gee, coxph, gls) and produces a summary table suitable for a
  report, allowing you to provide a character vector to label the
  parameters meaningfully.
- `km_ggplot` is a function that takes a `survfit` object and produces a
  publication-quality fig of the Kaplan-Meier plot with error-bands plus
  a table counting numbers at risk and with events.
- `propercase`
- `rbind_space` : to glue together repeated outputs from `sumby`
- `clean_up` is called by default within each call of `write_*`. They
  have arguments to not call `clean_up` if the code is such that you
  need to re-use the same R objects for multiple outputs.
- `.reserved` is a hidden variable in the global environment. `clean_up`
  ignores any objects named in `.reserved`. So you can edit this as an
  alternative to using the arguments within `write_*`, but ideally it
  should be defined once at the end of data manipulation.
- `rm_envir` maybe helpful during code development and interactive use
  of `attach_pop`.

### Missing Data

With the use of `cttab` there are created as a side-effect summaries of
missing data. See these functions

- `dump_missing_report`
- `get_missing_report`
- `reset_missing_report`

### Audit trails

As described already
[`source()`](https://cam-ctu.github.io/cctu/reference/source.md)
captures a trail of which code sources other code. There is a
demonstration below of how to convert this into a graphical tree
representation using a combination of Rmarkdown and latex. Or you can
just get a copy `cctu:::cctu_env$code_tree` and write a local copy to a
csv file say.

In a similar fashion we have some examples of Rmarkdown that can be use
to read in the outputs from `write_*` and produce a Html or pdf, or …
version of the main report, depending on what the readers of the report
prefer as their format.

At the end of your code it is good practice to run
`Sys.info(), date(), sessionInfo()` to document the fine details of your
session, package versions, etc. Also it may want to be run for the final
definitive version of the report on a different server with a validated
instance of R, using a `R CMD Batch` which has the nice side-effect of
creating a .Rout file with a complete log of all the code and outputs. A
wrapper function to avoid having to open a command line window for your
operating system is
[`run_batch()`](https://cam-ctu.github.io/cctu/reference/run_batch.md) .

## Worked Example

This document is used to illustrate how to set up a standard set of
analysis using the library cctu. It assumes that you have copied across
a template blank folder structure (`cctu_initialise`), created a library
within the main folder with all the extra packages you may need, and set
up a git versioning instance and rstudio project. A future project will
be to document this step.

In the the top level this is a file called “main.R”

### Initial lines

``` r
rm(list = ls())
# if using renv for extra packages need this line for R CMD BATCH
# renv::load()
library(cctu)
#> 
#> Attaching package: 'cctu'
#> The following object is masked from 'package:base':
#> 
#>     source
options(verbose = TRUE)
# run_batch("main.R")
DATA <- "PATH_TO_DATA"
cctu_initialise()
rm_output()
```

If you run just these initial lines, the last command will evoke R CMD
BATCH to run the entire set of code and produce a log file “main.Rout”,
which should be the final step, using the validated server. The
`run_batch` line is commented out as the vignette will not work with
this though..

This vignette now differs from a standard use, in that the `Main.R` file
would now be a sequence of
[`source()`](https://cam-ctu.github.io/cctu/reference/source.md) calls.
Here we do run the source files, and then quote the R code they contain.
There is a copy of all files and outputs from a standard use starting
from [Main.R](https://cam-ctu.github.io/cctu/articles/Main.R)

### Configuration

It is recommended to set up a config.R file that

- reads in all your libraries
- sets up graphical settings
- maybe reads in some bespoke functions you want to define

#### Library management

If you use the Docker image `shug0131/cctu` then it should be a rare
event that you need to install any extra packages. But if you do then it
preferable to manage this using `renv` package. Best practice is to
write a list of the packages needed, including any extra ones, in a
DESCRIPTION file at the top level, listing them under the `Imports:`
heading. Running `cctu::initialise()` will by default create this
DESCRITPTION file and remind you to edit it.

Then you modify the script in `config.R` to read this file and workout
which packages to load. The function
[`library_description()`](https://cam-ctu.github.io/cctu/reference/library_description.md)
will do this step for you.

If you do need to use `renv` for installing extra packages, then adding
them to DESCRIPTION will make this all very explicit, and will help
create the `renv.lock` file correctly to get the package versions of
everything that you need.

``` r
source("Progs/config.R")
```

``` r
options(verbose = TRUE)

library_description()

# mylibs <- c("readxl", "dplyr", "ggplot2", "magrittr","tidyr","rmarkdown","knitr","xml2","rvest")
#
# for(package in mylibs){
#   library(package, character.only = TRUE)
# }


# read in all function files
my_functions <- list.files("Progs\\functions")
for(file in my_functions){
  if( file != "archive"){
  source(paste0("Progs\\functions\\", file))
  }
}

# define theme for figures
default_theme <- theme_get()

graphical_theme <- theme_bw() + theme(
  axis.line.x      = element_line(color = "black"),
  axis.line.y      = element_line(color = "black"),
  panel.grid.major = element_blank() ,
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  # panel.border = element_blank(),
  # axis.text = element_text(size = rel(1), angle = 45)
  axis.title.x     = element_text(margin = margin(t = 10)),
  legend.key       = element_rect(colour = "white", fill = NA),
  strip.background = element_rect(colour = "black")
)

# remove anything no longer required
rm(mylibs, package, my_functions, file)
```

### Data Import

Next step is to import the meta-table, study data and apply
manipulations, and finally create the population environments.

- Grab data from the “DATA” folder. Always use relative paths, or build
  absolute paths up from MACRO variables defined once near the start
  ‘paste0(DATA, “/myfile.csv”)’
- convert the variable names into standard ‘lower_case’ so you have to
  remember less
- remove blank rows and columns
- give factor variables their levels
- make dates into dates if needed, whilst being careful of partial
  dates…

Here we grab a ready prepared ‘dirty’ raw data typical of MACRO DB and
apply some of these concepts.

``` r
source("Progs/data_import.R")
```

``` r
options(stringsAsFactors = FALSE)

data_table <- data.frame(
  name=c("data","codes"),
  file=c("dirtydata.csv",
         "codes.csv"),
  folder=system.file("extdata", package="cctu"),
  stringsAsFactors = FALSE
)


read_data(data_table)
read_data( data_table[2,])

# take an template meta table and add a new row with edited values.
meta <- cctu::meta_table_example
regress <- meta[1,]
regress$title <- "Regression: Age predicted by Treatment"
regress$number <- "1.2"
meta <- rbind(meta, regress)

km <- meta[1,]
km$title <- "Kaplan-Meier Plot"
km$number <- "1.3"
km$item <- "figure"
meta <- rbind(meta, km)
set_meta_table( meta)


write_table(data_table_summary(data_table),number = "9", clean_up = FALSE)


# Not strictly needed as the default is to apply these two functions inside of read_data(), but this
# can be turned off with clean_names_option = FALSE , or remove_blank_rows_cols_option=FALSE
data %<>% clean_names() %>% remove_blank_rows_cols()
codes %<>% clean_names


for( x in unique(codes$var)){
  code_df <-  subset(codes, var==x)
  print(code_df)
  data[,x] <- factor( data[,x], levels=code_df$code, labels=code_df$label)
}

data$start_date <- as.POSIXct( data$start_date , format="%d/%m/%Y")
data_name <- names(data)
names(data)[match("subject_id", data_name)] <- "subjid"



#Create the population table

popn <- data[,"subjid", drop=FALSE]
popn$safety <- TRUE
popn$full <- popn$subjid<5

create_popn_envir("data",popn)

#tidy up
rm(code_df, codes, data_name, x)
.reserved <- ls()
```

From here one could refer to the vignette `using-cttab`, and apply
further functions: `apply_macro_dict`, and/or `var_lab`

### Analysis

``` r
source("Progs/analysis.R")
```

``` r
attach_pop("1.1")
X <- rbind_space(
  sumby(age, treatment , data=data),
  sumby(gender, treatment, data=data)
)
write_table(X)

attach_pop("1.1.1")
X <- rbind_space(
  sumby(age, treatment, data=data, label="aGe (yAars)", text_clean = NULL),
  sumby(gender, treatment, data=data, text_clean = NULL)
)
write_table(X)


attach_pop("1.10")
X <- sumby(age, treatment, data=data)
write_ggplot(attr(X,"fig"))

attach_pop("1.2")
fit <- lm(age~treatment, data=data, na.action = na.omit)
X <- regression_table(fit, labels = c("Mean age reference arm","Treament Effect"))
#X <- sumby(age, treatment, data=data, label="aGe (yAars)", text_clean = NULL)
write_table(X)

attach_pop("1.3")
library(survival)
fit <- survfit(Surv(time,status)~rx, data=colon)
fig <- km_ggplot(fit)
write_plot(fig)

attach_pop("2")
write_text("There were no deaths")
```

### Creating the Report

Need to create names for the population labels, including the number of
subjects. Good to name the report ending with the suffix “.doc”.

``` r
pop_size <- sapply(popn[, names(popn) != "subjid"], sum)
pop_name <- unique(get_meta_table()$population)
index <- match(pop_name, names(pop_size))
popn_labels <- paste0(propercase(pop_name), " (n = ", pop_size[index], ")")

write.csv(get_meta_table(), file = file.path("Output", "meta_table.csv"), row.names = FALSE)
write.csv(get_code_tree(), file = file.path("Output", "codetree.csv"), row.names = FALSE)
write_docx(
  report_title = "Vignette Report",
  author = "Simon Bond",
  filename = file.path("Output", "Reports", "Vignette_Report.doc"),
  popn_labels = popn_labels
)
#> now dyn.load("/home/runner/work/_temp/Library/png/libs/png.so") ...
#> Output/Reports/Vignette_Report.docx created.
Sys.info()
#>                                               sysname 
#>                                               "Linux" 
#>                                               release 
#>                                   "6.11.0-1018-azure" 
#>                                               version 
#> "#18~24.04.1-Ubuntu SMP Sat Jun 28 04:46:03 UTC 2025" 
#>                                              nodename 
#>                                       "runnervmf2e7y" 
#>                                               machine 
#>                                              "x86_64" 
#>                                                 login 
#>                                             "unknown" 
#>                                                  user 
#>                                              "runner" 
#>                                        effective_user 
#>                                              "runner"
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] survival_3.8-3 xml2_1.4.1     rvest_1.0.5    tidyr_1.3.1    rmarkdown_2.30
#>  [6] readxl_1.4.5   magrittr_2.0.4 knitr_1.50     ggplot2_4.0.0  dplyr_1.1.4   
#> [11] cctu_0.8.9    
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10        generics_0.1.4     lattice_0.22-7     digest_0.6.37     
#>  [5] evaluate_1.0.5     grid_4.5.2         RColorBrewer_1.1-3 fastmap_1.2.0     
#>  [9] cellranger_1.1.0   jsonlite_2.0.0     Matrix_1.7-4       gridExtra_2.3     
#> [13] httr_1.4.7         purrr_1.2.0        scales_1.4.0       textshaping_1.0.4 
#> [17] jquerylib_0.1.4    cli_3.6.5          rlang_1.1.6        splines_4.5.2     
#> [21] xslt_1.5.1         withr_3.0.2        cachem_1.1.0       yaml_2.3.10       
#> [25] tools_4.5.2        png_0.1-8          vctrs_0.6.5        R6_2.6.1          
#> [29] lifecycle_1.0.4    htmlwidgets_1.6.4  ragg_1.5.0         pkgconfig_2.0.3   
#> [33] pillar_1.11.1      bslib_0.9.0        gtable_0.3.6       data.table_1.17.8 
#> [37] glue_1.8.0         Rcpp_1.1.0         systemfonts_1.3.1  xfun_0.54         
#> [41] tibble_3.3.0       tidyselect_1.2.1   farver_2.1.2       htmltools_0.5.8.1 
#> [45] patchwork_1.3.2    labeling_0.4.3     compiler_4.5.2     S7_0.2.0
date()
#> [1] "Thu Nov  6 15:59:15 2025"
```

The output is
[Output/Reports/Vignette_Report.doc](https://cam-ctu.github.io/cctu/articles/Output/Reports/Vignette_Report.docx).
To permanently save, first go to file \> Edit Links to Files; highlight
all the figures (shift + scroll), and click “break link”. Then File\>
save as, and ensure it is Saved As Type a “Word Document (\*.docx)“.

### Multiple Versions of Reports

Often you may need to produce slight variations on the same report, such
as a closed and open versions. We provide various options() to overwrite
the default arguments in `write_**()` with
`options(cctu_output="Other_Output_Folder")`, although you would still
need to create this directory and the required sub directories first.
Also you can provide `source(file, local=TRUE)` to allow scripts to be
run inside a function, rather than in the global environment;
`options(cctu_source_local=TRUE)` overrides the default. But then your
code will need to branch to produce different outputs (e.g. pooling arms
or not), depending which version. You can select subsets of the meta
table to remove/include tables or figures in a similar branching logic.

The idea is to be able to set some differing options and/or other R
objects in a single script that produces all versions of the report with
a repeated `source("main.R")`.

Alternative approaches have been found, code-to-write-code, using
[`knitr::knit_expand`](https://rdrr.io/pkg/knitr/man/knit_expand.html).
Creating functions to modifying the meta-table automatically. But not
without complexity!

You will still need to produce the log of all the commands with
[`run_batch()`](https://cam-ctu.github.io/cctu/reference/run_batch.md)
or just `system("R CMD BATCH main.R")` (N.B if you have extra packages
managed with `renv()` then you need to add a line `renv::load()` at the
start of your top-level script; don’t use `--vanilla` as this stops the
`renv` setup working ). You still need to produce the output of
cctu_env\$code_tree for all versions of the reports.

We’d recommend using a master script approach for simplicity and
readability, unless good reason not.

### Other Outputs

We can use rmarkdown to create other versions of the main report or a
graphical representation of the code architecture. You *may* need to use
`Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")`
before calling `render`, but this is not understood by the author at
present as to why.

Numerous other outputs can be obtained from rmarkdown: slide show of the
figures, reformatting of output as desired…

#### Code Tree

You can use the following example to produces a code tree pdf.

``` r
ctfile <- get_code_tree()
pdf("codetree.pdf", width=13, height=7)
plot(ctfile)
dev.off()
```

You can also use `write_plot` function to include the code tree plot in
your report.

#### Cross reference with Docx

Tables can be cross-referenced in the word. This is very convenient if
you are writing narratives and want to reference tables or figures. You
can find how to cross reference tables or figures
[here](https://erinwrightwriting.com/cross-reference-tables-and-figures-in-microsoft-word/).
If you can changed the order of the tables by cut and paste or by
dragging the headings in the navigation pane of the word the reference
will still be there. You can press `Ctrl+A` select all, and press `F9`.
This will change numbering of the table and figure, this change will
also be applied to the references you made in the narratives.

#### HTML version of the report

Your readers may prefer to get the report in a HTML document to view
on-screen. This provides easier navigation with a floating toolbar. See
the vignette `Vignette Report HTML`
