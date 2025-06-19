attach_pop(1.1)
X <- sumby(endpoint, rx, data = data)
write_table(1.1, X)

attach_pop(1.2)
X <- sumby(endpoint, rx, data = data)
write_table(1.2, X)



attach_pop(1.3)
sumby(response, rx, data = data)
# could actually just call the write_ggplot() now, but the line below is clearer
fig <- sumby(response, rx, data = data) %>% attr("fig")
write_ggplot(1.3, meta_table_string = "meta_subset", format = "jpeg", clean_up = FALSE)
write_ggplot(1.3, plot = fig, clean_up = TRUE)
