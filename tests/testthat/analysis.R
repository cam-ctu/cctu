
attach_pop(1.1)
ls()
print("hello")
summary(data)
X <- sumby(endpoint, rx, data=data )
write_table(X)#, directory="tests/testthat/Output/Core/")



attach_pop("1.1.1")
X <- sumby(endpoint, rx, data=data )
write_table(X, clean_up = FALSE)#, directory="tests/testthat/Output/Core/")

meta_table <- get_meta_table()

meta_table[2,"subtitle"] <- ""


meta_table[4,"number"] <- "1.1.2"
meta_table[4,"population"] <- " "
set_meta_table(meta_table)

write_table(X, number="1.1.2")


attach_pop("1.10")
sumby(response, rx, data=data )
#could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data=data ) %>% attr("fig")
write_ggplot( #directory="tests/testthat/Output/Figures/",
              format="png")

#source("analysisB.R", local=TRUE)

