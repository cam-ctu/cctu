
attach_pop(1.1)
X <- sumby(endpoint, rx, data=data )#directory=test_path("Output/Figures"))
write_table(X,directory=test_path("Output/Core"))



attach_pop("1.1.1",verbose=TRUE)
X <- sumby(endpoint, rx, data=data )#directory=test_path("Output/Figures"))
write_table(X, clean_up = FALSE,#directory=test_path("Output/Core"),
            heading = c("Variable","Statistics with a long line of text to check folding","A","B","Total")
)

meta_table <- get_meta_table()

meta_table[2,"subtitle"] <- ""

ntabs <- nrow(meta_table)
meta_table[ntabs+1,"number"] <- "1.1.2"
meta_table[ntabs+1,"population"] <- " "
set_meta_table(meta_table)

write_table(X, number="1.1.2")#directory=test_path("Output/Core"))


attach_pop("1.10")
sumby(response, rx, data=data)#directory=test_path("Output/Figures") )
#could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data=data) %>% attr("fig")#,directory=test_path("Output/Figures") ) %>% attr("fig")
write_ggplot( fig, format="png")#directory=test_path("Output/Figures"))

attach_pop("2.1")
write_text("There were no deaths")#directory=test_path("Output/Core"))

