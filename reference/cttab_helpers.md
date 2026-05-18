# Internal helpers shared by \`cttab\`, \`cttab_plot\` and \`report_missing\`.

Centralising these so the three call sites stay consistent - variable
label lookup, \`select\` filter evaluation and grouping-column factor
coercion all need to behave identically across the table, the plot and
the missing-data report.
