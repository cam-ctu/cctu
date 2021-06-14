## ----html_report , include=FALSE, message=TRUE, warning=FALSE-----------------
library(cctu)
library(magrittr)

PATH_figure <- file.path(getwd(),"Output","Figures")

meta_table <- read.csv(file=file.path("Output","meta_table.csv"), stringsAsFactors = FALSE)
index <- order_dewey(meta_table$number)
meta_table <- meta_table[index,]
rm(index)

sections <- unique(meta_table$section)
src <- as.character(NULL)
for(sect in sections){
  src <- c(src, paste("\n#", sect)) 
  subtab <- subset(meta_table, section==sect)
  for(row in 1:nrow(subtab)){
    src <- c(src, paste("\n##", subtab[row,"number"], subtab[row,"title"], "\nPopulation:", subtab[row,"population"]))
    if( !is.na(subtab[row,"subtitle"])){ src <- c(src, paste("\nSubtitle:",subtab[row,"subtitle"]))}
             
    if( subtab[row,"item"] %>% tolower=="table"){
          file_name <- file.path("Output","Core",paste0("table_",subtab[row,"number"],".xml") )
        # knit exapnd really doesn't like absolute paths or \ or \\
        if( file.exists(file_name)){
          src <- c(src, 
               knitr::knit_expand(file=file.path("Progs","table.Rmd") %>% normalizePath(),
                           chunk_name=paste0("table",subtab[row,"number"]),
                           file_name=file_name 
                           )
          )}
    }
    
    
    if( subtab[row,"item"] %>% tolower=="text"){
          file_name <- paste0("Output/Core/text_",subtab[row,"number"],".xml") 
        # knit exapnd really doesn't like absolute paths or \ or \\
        if( file.exists(file_name)){
          src <- c(src, 
               knitr::knit_expand(file="Progs/text.Rmd" %>% normalizePath(),
                           chunk_name=paste0("text",subtab[row,"number"]),
                           file_name=file_name 
                           )
          )}
    }
    
    
    if(subtab[row,"item"] %>% tolower=="figure" ){
      file_name <- file.path(PATH_figure,paste0("fig_",subtab[row,"number"],".png"))
      if( file.exists(file_name)) src <- c(src, paste0("![](",file_name,") \n" ))
    }
    if( !is.na(subtab[row,"footnote1"])){ src <- c(src, paste("\n",subtab[row,"footnote1"]))}
    if( !is.na(subtab[row,"footnote1"])){ src <- c(src, paste("\n",subtab[row,"footnote2"]))}
  }
}




## ----table1.1.1, echo=FALSE, results='asis', message=FALSE, warning=FALSE-----
tab <- xml2::read_html("Output/Core/table_1.1.1.xml")  %>%
  rvest::html_nodes("table") %>% 
  rvest::html_table(header=TRUE, fill=TRUE)
#Remove blank rows -Pandoc can't deal with them
tab <- tab[[1]]
blank <- apply(tab,1, function(x){ all(is.na(x)|trimws(x)=="")})
knitr::kable(tab[!blank,], format="html", row.names=FALSE) %>%
   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## ----table1.1, echo=FALSE, results='asis', message=FALSE, warning=FALSE-------
tab <- xml2::read_html("Output/Core/table_1.1.xml")  %>%
  rvest::html_nodes("table") %>% 
  rvest::html_table(header=TRUE, fill=TRUE)
#Remove blank rows -Pandoc can't deal with them
tab <- tab[[1]]
blank <- apply(tab,1, function(x){ all(is.na(x)|trimws(x)=="")})
knitr::kable(tab[!blank,], format="html", row.names=FALSE) %>%
   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## ----text2, echo=FALSE, results='asis', message=FALSE-------------------------
tab <-  xml2::read_html("Output/Core/text_2.xml")%>%
#tab <- read_html(paste0(PATH_table,"table_",2.1,".xml")) %>%
   rvest::html_nodes("text") %>% 
   rvest::html_text()
#Remove blank rows -Pandoc can't deal with them

cat("<p>", tab, "</p>")

## ----table9, echo=FALSE, results='asis', message=FALSE, warning=FALSE---------
tab <- xml2::read_html("Output/Core/table_9.xml")  %>%
  rvest::html_nodes("table") %>% 
  rvest::html_table(header=TRUE, fill=TRUE)
#Remove blank rows -Pandoc can't deal with them
tab <- tab[[1]]
blank <- apply(tab,1, function(x){ all(is.na(x)|trimws(x)=="")})
knitr::kable(tab[!blank,], format="html", row.names=FALSE) %>%
   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## ---- eval=FALSE--------------------------------------------------------------
#  
#  rmarkdown::render(file.path("Progs","rmarkdown_report.Rmd"),
#         output_file ="Vignette Report.html",output_dir = file.path("Output","Reports"),
#         params=list(my_author="Simon Bond",
#                     my_title= "Vignette Report")
#  )

