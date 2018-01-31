attach_pop(1.1)
X <- sumby(endpoint, rx, data=data )
write_table(1.1,X)



attach_pop(1.2)
sumby(response, rx, data=data )
#could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data=data ) %>% attr("fig")
write_ggplot(1.2, plot=fig, clean_up=FALSE)
write_ggplot(1.2, format="jpeg", clean_up=TRUE)
