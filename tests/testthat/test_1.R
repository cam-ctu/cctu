
### This is an early edevelopement test. Not needed now. 18 Jun2020.





#.libPaths(c(.libPaths(), "V:/STATISTICS/STUDY PLANNING/R_library"))
#.libPaths("V:/STATISTICS/STUDY PLANNING/R_library")
# Sys.setenv(R_LIBS_USER="V:/STATISTICS/STUDY PLANNING/R_libraryV3.5")

rm(list=ls())
context("Test the testing")
#library(cctu)
library(testthat)
library(readxl)
library(magrittr)

options(verbose=TRUE)

test_that("ccti_initialise",{
  unlink(test_path("Output"), recursive = TRUE)
  expect_warning(source(test_path("hello_world.R"), local=TRUE), "Recommend using cctu_initialise()")
}
)

cctu_initialize(root=test_path())

#PATH <- paste0(getwd(),"/tests/testthat/")
#PATH <- paste0(getwd(),"/")
#setwd(PATH)
#meta_table is a special name that is set as default in numerous other functions
meta_table <- read_excel(system.file("extdata", "meta_table.xlsx", package="cctu")) %>% as.data.frame(stringsAsFactors=FALSE) %>% clean_names
meta_table %<>% within(
  item %<>% tolower
) %>% dplyr::select(section, title, subtitle, number, population,
                      orientation, program, item, footnote1, footnote2,fontsize)
meta_subset <- meta_table[3,]
#devtools::use_data(meta_table, overwrite = TRUE)

set_meta_table(meta_table)

set.seed(1649)
data <- data.frame( subjid=1:100,
                    endpoint=rnorm(100) %>% round(2),
                    response=rep(c("Fail","Respond"),rep(50,2)),
                    rx=rep(c("A","B"),50)
                    )
popn <- data.frame(subjid=data$subjid,
                   safety=rep(TRUE,100),
                   full=rep(rep(c(TRUE,FALSE),c(9,1)),10)
)

create_popn_envir(c("data"), popn)

#Don;t actually need PATH defined or included in RESERVED as the current code stands!
.reserved<- c("meta_table","meta_subset","popn","safety","full","code_tree")


if(TRUE){attach_pop(1.1)
X <- sumby(endpoint, rx, data=data ,directory=test_path("Output/Figures"))
write_table(X, directory=test_path("Output/Core"))
#, directory="tests/testthat/Output/Core/")

attach_pop("1.1.1")
X <- sumby(endpoint, rx, data=data,directory=test_path("Output/Figures") )
write_table(X,directory=test_path("Output/Core")
            )#, directory="tests/testthat/Output/Core/")
#meta_table[1,"subtitle"] <- ""

#meta_table <- meta_table[c(1:3,2),]
#meta_table[4,"number"] <- "1.1.2"
#meta_table[4,"population"] <- " "

#write_table(X, number="1.1.2")


attach_pop("1.10")
sumby(response, rx, data=data ,directory=test_path("Output/Figures"))
#could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data=data ,directory=test_path("Output/Figures")) %>% attr("fig")
write_ggplot(directory=test_path("Output/Figures"))
}


#source("tests/testthat/analysis_interactive.R")
# code_tree
# meta_table
# meta_subset


# need the local =TRUE to call this from the testthat(). but not in general.


file.remove("backup_image.Rdata")
expect_equal(file.exists("backup_image.Rdata"), FALSE)
source(test_path("analysis.R"), local=TRUE, backup="backup_image.Rdata")
expect_equal(file.exists("backup_image.Rdata"), TRUE)
file.remove("backup_image.Rdata")
cctu_env$code_tree

test_that("exist",
          {
            expect_true(exists("popn"))
            expect_equal(get_obj("popn"),popn)
          }
          )

#create the correct population labels
# need to check the order of unique(meta_table$Population)

popn_size <- apply(popn[,names(popn)!="subjid"],2,sum)
popn_labels <- paste0(c("Safety (N = ","Full Analysis (N = "), popn_size,c(")", ")"))

#setwd(PATH)

# override Sys.time() to always return a fixed time to use in snapshots
local_mocked_bindings(
  Sys.time = function() as.POSIXct("1987-05-16 16:18:00 BST")
)

# now use snapshots...

test_that("create snapshots",{
expect_warning(
  create_word_xml("Test <Report>",
                "Simon & Bond's",
                get_meta_table(),
                datestamp="Test Date",
                popn_labels = popn_labels,
                filename=test_path("Output","Reports","Report2.doc"),
                table_path=test_path("Output/Core"),
                figure_path=test_path("Output/Figures")
                #xslt_file = system.file("extdata", "trial_xml_to_word.xslt", package="cctu")
                #table_path = "Output/Core",
                #figure_path="Output/Figures",
                ), "This function is no longer being maintained")


#expect_snapshot_file( test_path("Output","Reports","Report2.doc"), "Report2.doc")

write_docx("Test <Report>",
            "Simon & Bond's",
            get_meta_table(),
            popn_labels = popn_labels,
            filename = test_path("Output","Reports","Report_final.docx"),
           table_path=test_path("Output/Core"),
           figure_path=test_path("Output/Figures")
)

#expect_snapshot_file( test_path("Output","Reports","Report_final.docx"), "Report_final.docx")

#setwd("tests/testthat")
#create_word_xml("Test <Report>",
#                 "Simon & Bond's",
#                 meta_table,
#                 datestamp="Test Date",
#                 popn_labels = popn_labels
#                 )
#
#
# create_word_xml("Test Report Jpeg",
#                 "Simon Bond",
#                 meta_subset,
#                 datestamp="Test Date",
#                 filename="Output\\Reports\\ReportJpg.doc",
#                 figure_format="jpeg",
#                 popn_labels = popn_labels)

#out to write.csv(meta_table) to record the final version post code.
})


test_that("Creation of files",{
  expect_true(file.exists(test_path("Output","Reports","Report2.doc")))
  expect_true(file.exists(test_path("Output","Reports","Report_final.docx")))
  # expect_true(file.exists("Output/Reports/ReportJpg.doc"))
  expect_true(file.exists(test_path("Output","Core","table_1.1.1.xml")))
  expect_true(file.exists(test_path("Output","Core","table_1.1.xml")))
  #expect_true(file.exists("Output/Figures/fig_1.3.png"))
  expect_true(file.exists(test_path("Output","Figures","fig_1.10.png")))
})

test_that("get_code_tree",{
  write.csv(get_code_tree(), file=test_path("Output","codetree.csv"), row.names = FALSE)
  expect_true(file.exists(test_path("Output","codetree.csv")))
})




test_that("Comparison to saved output",{
  library(xml2)
  test <- read_xml(test_path("Output","Core","table_1.1.xml"))
  expect_known_output(test %>%as.character, test_path("data","table1.1"), update=TRUE, print=TRUE)
  # Won;t work given the explicit file path in the footnotes
  #test <- read_xml("Output/Reports/Report.doc")
  #expect_known_output(test %>%as.character, "data/Report", update=FALSE, print=TRUE)
  #test <- read_xml("Output/Reports/ReportJpg.doc")
  #expect_known_output(test %>%as.character, "data/ReportJpg", update=FALSE, print=TRUE)
})

print(cctu_env$code_tree)

#put this into a vignette

# documentation on the meta_table, and code to run a check on the meta_table.


#clean names
# source
#remove blank rows cols
#rm_envir
#code_tree tests and make the pdf file


# put in the R CMD BAtch to create the log file..
# do the rmarkdown tempaltes to create the html version.

#maybe
#srce
#sumby_ae


