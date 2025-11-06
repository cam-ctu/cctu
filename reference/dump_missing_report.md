# Save/Get/Reset missingness report

`dump_missing_report` can be used to save the missingness report to a
file. `get_missing_report` Return the missingness report data.
`reset_missing_report` Reset the internal missingness report data to
blank.

## Usage

``` r
dump_missing_report(x = "Output/variable_missing_report.csv")

get_missing_report()

reset_missing_report()
```

## Arguments

- x:

  File path the report will be dumped to. Default is under \`Output\`
  folder, named as \`variable_missing_report.csv\`.

## See also

[`cttab`](https://cam-ctu.github.io/cctu/reference/cttab.md)
[`report_missing`](https://cam-ctu.github.io/cctu/reference/report_missing.md)
