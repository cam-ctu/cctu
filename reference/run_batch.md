# Runs a script in batch mode directly from within R.

This function is to allow R CMD BATCH to be called interactively,
without needing to start up a command line terminal and change
directories etc. It calls the R CMD BATCH from the current working
directory. Hence its value is in producing the side-effects: whatever
happens when the script is run. Plus a log file will be created in the
working directory with the same name as the script but with the suffix
changed to '.Rout'.

## Usage

``` r
run_batch(filename, ...)
```

## Arguments

- filename:

  character string of the name or path of the script (relative to the
  working directory).

- ...:

  other arguments to pass to
  [`system`](https://rdrr.io/r/base/system.html)

## Value

see [`system`](https://rdrr.io/r/base/system.html) which is called.

## Details

The command will not run in environments where
[`interactive`](https://rdrr.io/r/base/interactive.html) is false, so as
to prevent infinite loops.

## See also

[`BATCH`](https://rdrr.io/r/utils/BATCH.html) and `rcmd` from the callr
package, which might already do this....

`check_failures` from the devtools package for future ideas for a
function to check the log file for errors or warnings.
