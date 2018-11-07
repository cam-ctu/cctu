attach_pop(1.1)
ls()
print("hello")
summary(data)
X <- sumby(endpoint, rx, data=data )
write_table(1.1,X, directory="tests/testthat/Output/Core/")

attach_pop("1.1.1")
X <- sumby(endpoint, rx, data=data )
write_table("1.1.1",X, clean_up = FALSE, directory="tests/testthat/Output/Core/")
meta_table[1,"subtitle"] <- ""

meta_table <- meta_table[c(1:3,2),]
meta_table[4,"number"] <- "1.1.2"
meta_table[4,"population"] <- " "

write_table("1.1.2",X, directory="tests/testthat/Output/Core/")
#need to manually clean_up, as 1.1.2 has no population
clean_up("1.1.1")

attach_pop("1.10")
sumby(response, rx, data=data )
#could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data=data ) %>% attr("fig")
write_ggplot("1.10", #directory="tests/testthat/Output/Figures/",
             meta_table_string="meta_subset",
             format="png", directory="tests/testthat/Output/Figures/")

#source("analysisB.R", local=TRUE)
