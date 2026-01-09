To create a file in a vignette (that's not the html report) you need to

a) run the vignette directly from the .Rmd file, to create the file in the vignette folder.
b) change the name of the file e.g add "_bak" onto the file name.
Else it will get deleted during the build process
c)  use a link like  [my file](../doc/my_file_bak.docx) in the .Rmd vignette file.
d)  in the .install_extras add a line like  .*[.]docx$  to copy all docx files.
I've not yet succeeded to make this work with subfolders- Keep it simple
e)  then with devtools::build_vignettes()  you should get this copied across into /doc
and the cross-reference works.  The /doc folder is what you see in the install package.


To make the rmarkdown_report.Rmd work. Requires

a) run analysis-template.Rmd manually
b) run the individual blocks of R code from rmark_down_report.Rmd
(but not the first one to copy the meta_table.csv - you already have this)

Then the build will copy across the output files. The rmark_down_report.Rmd doesn't evaluate
the R code, as it is fragile...






