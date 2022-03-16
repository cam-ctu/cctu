#' @name to_factor
#' @rdname to_factor
#'
#' @title Convert vector but keep value/value labels.
#'
#' @param x vector
#' @param ordered logical flag to determine if the levels should be regarded as ordered.
#' @param drop.levels If `TRUE`, drop unused levels from the converted factor, default
#' is `FALSE`.
#' @return Factor/character of the same form as x but with value labels instead
#' of values. For numeric and logical vectors with corresponding type.
#'
#' @seealso \link{val_lab},  \link{var_lab}
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(am) = "Transmission"
#'                 val_lab(am) = c(" automatic" = 0, " manual" =  1)
#' })
#'
#' mtcars$am <- to_factor(mtcars$am)
#' mtcars$gear <- to_character(mtcars$gear)
NULL


#' \code{to_factor} convert vector to factor with corresponding value labels or
#' unique values in the vector. Note that the `NA` or blank values will be excluded
#' from converting. It will return a factor with original values if there's no
#' value label.
#' @rdname to_factor
#' @export

to_factor <- function(x, ordered = TRUE, drop.levels = FALSE){

  if(!is.null(val_lab(x))){
    vallab <- val_lab(x)
  }else{
    vallab <- sort(unique(na.omit(x[trimws(x)!=""])))
    names(vallab) <- vallab
  }

  res <- factor(unlab(x),
                levels = vallab,
                labels = names(vallab),
                ordered = ordered)
  if(drop.levels)
    res <- droplevels(res)

  var_lab(res) = var_lab(x)
  res
}

#' \code{to_character} Convert vector to character vector and keep the variable
#' labels. The generic function `as.character` will lost the label attributes. If
#' you want to convert the value label to value itself, use \code{lab2val} instead.
#' @rdname to_factor
#' @export

to_character <- function(x){

  if(!is.null(val_lab(x))){
    vallab <- val_lab(x)
  }else{
    vallab <- sort(unique(na.omit(x[trimws(x)!=""])))
    names(vallab) <- vallab
  }
  res <- as.character(x)
  val_lab(res) <- vallab
  var_lab(res) <- var_lab(x)
  res
}

#' \code{to_numeric} Convert vector to numeric vector and keep the variable labels.
#' The generic function `as.numeric` will lost the label attributes.
#' @rdname to_factor
#' @export

to_numeric <- function(x){
  res <- as.numeric(x)
  var_lab(res) <- var_lab(x)
  res
}

#' \code{to_logical} Convert vector to logical vector and keep the variable labels.
#' The generic function `as.logical` will lost the label attributes.
#' @rdname to_factor
#' @export
to_logical <- function(x){
  res <- as.logical(x)
  var_lab(res) <- var_lab(x)
  res
}

