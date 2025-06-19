rm(list=ls())
#set to the library folder
.libPaths()
library(cctu)
options(verbose=TRUE)
#run_batch("main.R")
DATA <- "PATH_TO_DATA"
cctu_initialise()

source("Progs/config.R")
source("Progs/data_import.R")
source("Progs/analysis.R")


pop_size <- sapply( popn[,names(popn)!="subjid"], sum)
pop_name <- unique(get_meta_table()$population)
index <- match(pop_name, names(pop_size))
popn_labels <- paste0(propercase(pop_name), " (n = ", pop_size[index],")")


create_word_xml(report_title="Vignette Report",
                author="Simon Bond", filename="Output/Reports/Vignette_Report.doc",
                popn_labels=popn_labels
)

write.csv(get_meta_table(), "Output/meta_table.csv", row.names = FALSE)
write.csv(cctu:::cctu_env$code_tree, "Output/codetree.csv", row.names = FALSE)


Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
render("Progs/code_tree_doc.Rmd", output_file ="Code Tree.pdf", output_dir = "Output/Reports",
       params=list(my_author="Simon Bond",
                   my_title= "Code Tree: CCTU Vignette")

)
render("Progs/rmarkdown_report.Rmd",
       output_file ="Vignette Report.html",output_dir = "Output/Reports",
       params=list(my_author="Simon Bond",
                   my_title= "Vignette Report"),
       knit_root_dir=".."
)


Sys.info()
sessionInfo()
date()

