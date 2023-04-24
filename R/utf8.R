#' Functions to detect and delete non-UTF8 characters, which the XML output will not like
#'
#' @param data  a data.frame or a matrix, typically the input to \code{write_table}
#'
#' @return \code{detect_invalid_utf8} returns a data.frame  or a matrix that gives the column, row,
#' and value of any dataum points that contain invalid characters, or a zero-length
#' data frame if none are present.  \code{remove_invalid_utf8} replaces any invalid characters
#' with an empty string.
#'
#' @seealso \code{\link{write_table}}
#' @export

detect_invalid_utf8 <- function(data){
  if( !inherits(data, c("data.frame", "matrix"))){stop("input a data.frame or matrix")}
  X <- lapply(data,invalid_utf8_)
  where <- lapply(X, which)
  col_names <- names(where)
  cols <- rep(col_names, lapply(where, length))
  rows <- unlist(where)
  X_array <- array(unlist(X), dim=dim(data))
  # Tibble has issues, convert to data.frame
  data <- as.data.frame(data)
  data.frame(column=cols, row=rows, value=unlist( data[X_array]), row.names=NULL)
}

#' @describeIn detect_invalid_utf8 Function to delete invalid UTF8 charachters
#' @export
remove_invalid_utf8 <- function(data){
  if( !inherits(data, c("data.frame", "matrix"))){stop("input a data.frame or matrix")}
  X <- lapply(data, function(x){ iconv(x, "UTF-8", "UTF-8",sub='')})
  X <- lapply(X, function(x){gsub("\u00A0","\u0020",x)})
  data.frame(X)
}


#' @keywords internal

invalid_utf8_ <- function(x){
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")) |
    grepl("\u00A0",x)
}

