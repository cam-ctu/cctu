## ----include=FALSE------------------------------------------------------------
library(magrittr)
library(knitr)

code_tree <- read.csv(file.path("Output","codetree.csv"), stringsAsFactors = FALSE)


root <- code_tree[1,1]

PATH <- gsub("\\\\ROOT$", "", root,perl=TRUE) 

code_tree %<>% within({
  parent %<>% gsub("\\\\","/",., fixed=TRUE) %>% gsub(PATH,"",  . ,fixed=TRUE)
  child %<>% gsub("\\\\","/",., fixed=TRUE) %>% gsub(PATH,"",  . ,fixed=TRUE)
})

graphcode <- function( edges, node, rootnode=TRUE){
  #edges is a data.frame with two columns: parent, child
  if(length(node)){
    children <-  subset(edges, parent==node)$child
    if(!rootnode){ output <- " child{ "}
    if(rootnode){ output <- "" }
    output <- c(output,  "node{ ", node,"} ")
    #if(rootnode){ output <- c(output, "[grow=right] ")}
    for( child in children){ 
      output <- c(output, Recall(edges, child, rootnode=FALSE))
    }
    if(!rootnode){ output <- c( output, " }")}
  } else{ 
    output <- ""
  }
  output
}



graph <- graphcode(code_tree, code_tree[1,1])
graph <- gsub("\\\\","/", graph)
graph <- gsub(PATH,"", graph)
graph <- gsub("_"," ", graph)
graph <- paste(c('\\',graph, ";"), collapse = "")
graph

#sibling distance=20em,

## ---- eval=FALSE--------------------------------------------------------------
#  render("Progs/code_tree_doc.Rmd",
#         output_file ="Vignette Code Tree.pdf",output_dir = "Output/Reports",
#         params=list(my_author="Simon Bond",
#                     my_title= "Vignette Code Tree")
#  
#  )

