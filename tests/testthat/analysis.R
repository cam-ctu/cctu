attach_pop(1.1)
X <- sumby(endpoint, rx, data=data )
write_table(1.1,X)

#error code up to here

# traceback()
# 10: exists(name)
# 9: get_obj(meta_table_string)
# 8: .eval_pop(number, function_name = "detach", ...)
# 7: detach_pop(number, envir, ...)
# 6: clean_up(number, envir = parent_frame, ...)
# 5: write_table(1.1, X) at analysis.R#3
# 4: eval(ei, envir)
# 3: eval(ei, envir)
# 2: withVisible(eval(ei, envir))
# 1: source("analysis.R", echo = TRUE)




attach_pop(1.2)
fig <- sumby(response, rx, data=data ) %>% attr("fig")
write_ggplot(1.2,fig)
