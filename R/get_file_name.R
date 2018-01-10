###Function to get the filename and path of the most deeply nested call to source(), or whatever function in pattern
### ... are arguments to normalizePath()
### author: simon bond
### 20DEC2017

get_file_name <- function(pattern="source",...){
  calls <- sys.calls()
  if(!is.null(calls)){
    commands <- sapply(calls,function(x){as.character(x)[1]})
    index <- grep(pattern,commands)
    if(length(index)){
      normalizePath(as.character(calls[[max(index)]])[2], ...) 
    } else{
      NULL
    } 
  } else{
    NULL
  }
}
