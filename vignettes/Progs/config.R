

options(verbose = TRUE)

library_description()

# mylibs <- c("readxl", "dplyr", "ggplot2", "magrittr","tidyr","rmarkdown","knitr","xml2","rvest")
#
# for(package in mylibs){
#   library(package, character.only = TRUE)
# }


# read in all function files
my_functions <- list.files("Progs\\functions")
for(file in my_functions){
  if( file != "archive"){
  source(paste0("Progs\\functions\\", file))
  }
}

# define theme for figures
default_theme <- theme_get()

graphical_theme <- theme_bw() + theme(
  axis.line.x      = element_line(color = "black"),
  axis.line.y      = element_line(color = "black"),
  panel.grid.major = element_blank() ,
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  # panel.border = element_blank(),
  # axis.text = element_text(size = rel(1), angle = 45)
  axis.title.x     = element_text(margin = margin(t = 10)),
  legend.key       = element_rect(colour = "white", fill = NA),
  strip.background = element_rect(colour = "black")
)

# remove anything no longer required
rm(mylibs, package, my_functions, file)

