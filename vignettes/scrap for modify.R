The equivalent code in real life would be which does create a seperate [pdf document](Output/Reports/Code Tree.pdf) .

```{r render_code_tree, results=FALSE, warning=FALSE}
render("Progs/code_tree_doc.Rmd", output_file ="Code Tree.pdf", output_dir = "Output/Reports",
       params=list(my_author="Simon Bond",
                   my_title= "Code Tree: CCTU Vignette")
)
```

### HTML version

The following markdown reads back in the xml tabels and figures, and creates a html version of the report as a [seperate document](Output/Reports/Vignette Report.html)

```{r render_html, results=FALSE, warning=FALSE}

render("Progs/rmarkdown_report.Rmd",
       output_file ="Vignette Report.html",output_dir = "Output/Reports",
       params=list(my_author="Simon Bond",
                   my_title= "Vignette Report"),
       knit_root_dir=".."
)
```

date: "`r format(Sys.time(), '%d %B, %Y, %H:%M')`"
header-includes:
  - \usepackage{tikz}
  - \usetikzlibrary{trees,arrows}
output: pdf_document
classoption: landscape
params:
  my_title: "You only live twice"
  my_author: "Ian Flemming"
title: "`r params$my_title`"
author: "`r params$my_author`"
