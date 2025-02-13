(calls <- sys.calls())
commands <- sapply(calls, function(x) {
  as.character(x)[1]
})
commands
index <- grep("source(?!_)", commands, perl = TRUE)
index
as.character(calls[[max(index)]])[2]

get_file_name()
