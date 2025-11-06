# create environments contain a set of data frames filtered for each population

create environments contain a set of data frames filtered for each
population

## Usage

``` r
create_popn_envir(
  data_names,
  popn,
  subjid_string = "subjid",
  rm_from_frame = TRUE,
  frame = parent.frame(),
  verbose = options()$verbose
)
```

## Arguments

- data_names:

  a character vector of the key data frames to be filtered

- popn:

  a data frame one-row per participant a subjid variable and a column
  for each population indicating if a participant belongs to the
  population. The names of the variables must match up to the values in
  meta_table\$Population.

- subjid_string:

  the character naming the column used in each data frame from
  data_names and popn, to identify the subjid.

- rm_from_frame:

  a logical indicating if the data sets should be removed from the frame
  environment

- frame:

  the frame in which the original list of data sets are found, and in
  which the new environments will be contained.

- verbose:

  logical to print information on changes to the global environment or
  external files. Defaults to options()\$verbose.

## Details

Now to use the data frames defined above in data_names you have to
either directly call attach(safety), for example or look-up with
attach_pop(1.01) for a table number. There are separate R environments
made for each of the populations in population & population_title be
careful to use detach(safety) - this is automated with write_table() and
write_ggplot()
