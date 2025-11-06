# Set/get DLU data

`set_dlu` will set the provided DLU data to package environment, so it
can be used for missing data report by
[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md). It is
user's responsibility to make sure values of the short code in the
provided DLU data matches the variable names in the dataset. `set_dlu`
will not have any effect on
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md).
Instead, the
[`apply_macro_dict`](https://cam-ctu.github.io/cctu/reference/apply_macro_dict.md)
will override the \`DLU\` seetings done by `set_dlu`.

`get_dlu` can be used to get a copy of DLU data in stored by `set_dlu`.

## Usage

``` r
set_dlu(x, clean_names = TRUE)

get_dlu()
```

## Arguments

- x:

  DLU data

- clean_names:

  Conver variable name to lower case (default). See
  [`clean_names`](https://cam-ctu.github.io/cctu/reference/clean_names.md)
  for details.
