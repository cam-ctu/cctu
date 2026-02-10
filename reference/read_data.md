# Automatic reading in data from a meta-table of external data sets.

Automatic reading in data from a meta-table of external data sets.

## Usage

``` r
read_data(x, ...)

# S3 method for class 'data.frame'
read_data(x, name_variable = "name", file_variable = "file", ...)

# S3 method for class 'character'
read_data(
  x,
  data_table,
  fun = NULL,
  frame = parent.frame(),
  name_variable = "name",
  file_variable = "file",
  clean_names_option = FALSE,
  remove_blank_rows_cols_option = FALSE,
  ...
)
```

## Arguments

- x:

  character string or data.frame. If it is a character then it is the
  name of the object to be created, and referenced within the data to
  find the file path. If it is a dataframe then read_data is repeated
  across all the rows of the data.frame.

- ...:

  other arguments to supply to `fun`.

- name_variable:

  character string giving the variable name within `data` that has the
  object names to be referenced. Defaults to "name".

- file_variable:

  character string giving the variable name within `data` that has the
  file names to be referenced. Defaults to "file".

- data_table:

  data frame containing the meta-table of file paths of the external
  data files, and their desired R object names.

- fun:

  the function to be used to read in the data file. If unspecified it
  picks up file extensions ".xsl" and ".xslx" to use
  [`readxl::read_xls`](https://readxl.tidyverse.org/reference/read_excel.html)
  and
  [`readxl::read_xlsx`](https://readxl.tidyverse.org/reference/read_excel.html),
  otherwise uses `read.csv`. This could actually be any function applied
  to the file path character string that is extracted from `data_table`,
  but a warning is issued if the function name does not contain "read".

- frame:

  Environment in which an object with name given by `x` is created.
  Default is parent.frame(). Or if NULL the data read in is returned
  with no assignment.

- clean_names_option:

  logical to apply the
  [`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
  function internally. Defaults to `FALSE` for compatibility with
  [`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md).

- remove_blank_rows_cols_option:

  logical to apply the
  [`remove_blank_rows_cols`](https://cam-ctu.github.io/cctu/reference/remove_blank_rows_cols.md)
  function internally. Defaults to `FALSE` for compatibility with
  [`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md).

## Value

`read_data` assigns or returns a data frame reading in data from an
external file

## Details

The idea is to improve the tracibility of reading in external data. This
should be used in two steps: create a meta-table in R that has a minimum
of 2 columns, one with the name of the R data.frame to be created, and
the other giving the file path to the external data; use `read_data` as
a wrapper to read in the data as specified. This ends up with less code,
and allows an table of extenral data and associated meta-data to be
easily produced using `data_table_summary`. If options("verbose") is
`TRUE` then `read_data` will display messages describing what objects
have been created.

This is a generic method with methods defined for a character string,
and a data.frame. The former just reads in one data.frame, the latter
reads in all the data.frames specified.

## Methods (by class)

- `read_data(data.frame)`: data.frame method for read_data generic

- `read_data(character)`: character method for read_data generic

## See also

[`read.csv`](https://rdrr.io/r/utils/read.table.html)
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
[`data_table_summary`](https://cam-ctu.github.io/cctu/reference/data_table_summary.md)
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
[`var_lab`](https://cam-ctu.github.io/cctu/reference/var_lab.md)
[`extract_form`](https://cam-ctu.github.io/cctu/reference/extract_form.md)

## Examples

``` r
data_table <- data.frame(
  name = c("dirtydata", "meta"),
  file = c("dirtydata.csv", "meta_table.xlsx"),
  folder = system.file("extdata", package = "cctu"),
  stringsAsFactors = FALSE
)
data_table_summary(data_table)
#>        name            file                                       folder
#> 1 dirtydata   dirtydata.csv /home/runner/work/_temp/Library/cctu/extdata
#> 2      meta meta_table.xlsx /home/runner/work/_temp/Library/cctu/extdata
#>              mod_time
#> 1 2026-02-10 08:20:34
#> 2 2026-02-10 08:20:34
#>                                                 full_file_path
#> 1   /home/runner/work/_temp/Library/cctu/extdata/dirtydata.csv
#> 2 /home/runner/work/_temp/Library/cctu/extdata/meta_table.xlsx
options("verbose" = TRUE)
read_data(data_table)
#> object created in : dirtydata
#> now dyn.load("/home/runner/work/_temp/Library/readxl/libs/readxl.so") ...
#> object created in : meta
summary(dirtydata)
#>    Subject_Id        age           gender            Treatment  
#>  Min.   :1.00   Min.   :19.00   Length:8           Min.   :1.0  
#>  1st Qu.:2.25   1st Qu.:24.50   Class :character   1st Qu.:1.0  
#>  Median :3.50   Median :40.00   Mode  :character   Median :1.5  
#>  Mean   :3.50   Mean   :38.33                      Mean   :1.5  
#>  3rd Qu.:4.75   3rd Qu.:51.75                      3rd Qu.:2.0  
#>  Max.   :6.00   Max.   :56.00                      Max.   :2.0  
#>  NA's   :2      NA's   :2                          NA's   :2    
#>   start_date           outcome    missing        comments      
#>  Length:8           Min.   :0.0   Mode:logical   Mode:logical  
#>  Class :character   1st Qu.:0.0   NA's:8         NA's:8        
#>  Mode  :character   Median :0.5                                
#>                     Mean   :0.5                                
#>                     3rd Qu.:1.0                                
#>                     Max.   :1.0                                
#>                     NA's   :2                                  
summary(meta)
#>   Completed         Checked          Section             Title          
#>  Length:4           Mode:logical   Length:4           Length:4          
#>  Class :character   NA's:4         Class :character   Class :character  
#>  Mode  :character                  Mode  :character   Mode  :character  
#>                                                                         
#>                                                                         
#>                                                                         
#>                                                                         
#>    Subtitle            Number           Population        Orientation       
#>  Length:4           Length:4           Length:4           Length:4          
#>  Class :character   Class :character   Class :character   Class :character  
#>  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#>                                                                             
#>                                                                             
#>                                                                             
#>                                                                             
#>  Program            Item             Junk         Endpoint      
#>  Mode:logical   Length:4           Mode:logical   Mode:logical  
#>  NA's:4         Class :character   NA's:4         NA's:4        
#>                 Mode  :character                                
#>                                                                 
#>                                                                 
#>                                                                 
#>                                                                 
#>  Time Points or how to conglomerate Covariates or Subgroups Summary Statistics
#>  Length:4                           Length:4                Length:4          
#>  Class :character                   Class :character        Class :character  
#>  Mode  :character                   Mode  :character        Mode  :character  
#>                                                                               
#>                                                                               
#>                                                                               
#>                                                                               
#>  Formal Analysis  Footnote1         Footnote2         fontsize 
#>  Mode:logical    Length:4           Mode:logical   Min.   :16  
#>  NA's:4          Class :character   NA's:4         1st Qu.:17  
#>                  Mode  :character                  Median :18  
#>                                                    Mean   :18  
#>                                                    3rd Qu.:19  
#>                                                    Max.   :20  
#>                                                    NA's   :1   
```
