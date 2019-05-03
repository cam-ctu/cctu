There is a problem in creating output within the Vignettes directory and saving it when the package is
built.

.intall_extras does not seem to work. It does keep Progs/, but this only has .R or .Rmd docs inside..

_so_  first run

devtools::build_packages(clean=FALSE)

then copy the vignettes/Output directory into ~/inst

then

devtools::build() to get the tar.gz source

then install from tar.gz

and there will be a top-level Output/ directory with the outputs, and doc and xml files.

Could modify the links in teh vignette??? and or force the moving of the directory to inst when the vignette is built??




