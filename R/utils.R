
#' Combine descriptive statistics table by rows
#'
#' @param ... \code{stat_tab} descriptive statistics table .
#'
#' @export
#'
rbind.cttab <- function(...){
  allargs <- list(...)
  allargs <- Filter(Negate(is.null), allargs)

  all_class <- sapply(allargs, function(x)inherits(x, "cttab"))
  if(any(!all_class))
    stop("Only cttab class is supportted.")

  pos <- unlist(lapply(allargs, function(x)attr(x, "position")))
  obj_class <- unique(unlist(lapply(allargs, class)))

  # Remove cttab class for rbind
  allargs <- lapply(allargs, function(x){
    l <- which(attr(x, "class") == "cttab")
    attr(x, "class") <- attr(x, "class")[-l]
    return(x)
  })

  structure(do.call(base::rbind, allargs),
            position = pos,
            class = obj_class)

}


