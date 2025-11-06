# Functions to handle meta_table

Functions to handle meta_table

## Usage

``` r
get_meta_table()

set_meta_table(meta_table)
```

## Arguments

- meta_table:

  a data.frame to be set as the meta_table internal object

## Value

get_table returns the global object, set_table invisibly returns the
previous version

## Details

`set_meta_table(NULL)` will remove the meta_table .

## Functions

- `get_meta_table()`: gets a copy of the internal object

- `set_meta_table()`: sets the internal object to the argument provided,
  whilst carrying out some cleaning
