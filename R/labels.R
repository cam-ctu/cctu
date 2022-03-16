
#' Set or get variable label
#'
#' @description These functions set/get/drop variable labels. For
#' value labels see \link{val_lab}.
#' \itemize{
#' \item{\code{var_lab}}{ returns variable label or NULL if label doesn't
#' exist.}
#' \item{\code{var_lab<-}}{ set variable label.}
#' \item{\code{drop_lab}}{ drops variable label.}
#' \item{\code{has.label}}{ check if variable label exists.}
#' }
#' @param x Variable. In the most cases it is numeric vector.
#' @param value A character scalar - label for the variable x.
#' @return \code{var_lab} return variable label. If label doesn't exist it return
#'   NULL . \code{var_lab<-} return variable (vector x) with attribute "label" which equals submitted value.
#' @details Variable label is stored in attribute "label" (\code{attr(x,"label")}).
#' To drop variable label use \code{var_lab(var) <- NULL} or \code{drop_lab(var)}.
#' @export
#' @references
#' This is a modified version from `expss` package.
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "V/S"
#'                 val_lab(vs) = c("V-shaped" = 0, "straight"=1)
#'                 var_lab(am) = "Transmission"
#'                 val_lab(am) = c(automatic = 0, manual=1)
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#'
#' table(mtcars$am)
#'
#'
var_lab <- function(x) {
    UseMethod("var_lab", x)
}

#' @export
var_lab.default <- function (x) {
    y = attr(x, "label", exact = TRUE)
    if (is.null(y))
        return(NULL)
    y
}

#' @rdname var_lab
#' @export
var_lab.data.frame <- function(x) {
    lapply(x, var_lab)
}

#' @rdname var_lab
#' @export
`var_lab<-` <- function(x, value) {
    UseMethod("var_lab<-", x)
}


#' @export
`var_lab<-.default` <- function(x, value) {

  if ((!is.character(value) & !is.null(value)) | length(value) > 1)
    stop("`value` should be a single character string or NULL")

  if (length(value) == 0) {
    attr(x, "label") = NULL
    return(x)
  }

  if(!is.atomic(x))
    stop("`x` should be a atomic vector, not ", typeof(x))

  # this conversion is needed to avoid strange bug (incorrect residuals)
  # with 'lm' with labelled integers
  # if(is.integer(x)) x[] = as.double(x)
  value = as.character(value)
  if(length(value) != 1)
    stop("'var_lab' - label should be vector of length 1.")
  attr(x, "label") <- value
  x
}


#'@rdname var_lab
#' @export
has.label <- function(x) {
    !is.null(attr(x, "label", exact = TRUE))
}

#'@rdname var_lab
#' @export
drop_lab <- function(x) {
    UseMethod("drop_lab", x)
}

#' @export
drop_lab.default <- function(x) {
    attr(x, "label") <- NULL
    x
}

#' @export
drop_lab.data.frame <- function(x) {
    for (each in seq_along(x))
        x[[each]] = drop_lab(x[[each]])
    x
}


############# value labels #######################

#' Set or get value labels
#'
#' @description These functions set/get/drop value labels. Duplicated values are not allowed.
#' If argument \code{x} is data.frame or list then labels applied to all
#' elements of data.frame/list. To drop value labels, use \code{val_lab(var) <-
#' NULL} or \code{unval(var)}. For variable labels see \link{var_lab}.
#' \itemize{
#' \item{\code{val_lab}}{ returns value labels or NULL if labels doesn't
#' exist.}
#' \item{\code{val_lab<-}}{ set value labels.}
#' \item{\code{unval}}{ drops value labels.}
#' \item{\code{has.labels}}{ check if value labels exists.}}
#' @param x Variable(s). Vector/data.frame/list.
#' @param value Named vector. Names of vector are labels for the
#'   appropriate values of variable x. Names can be duplicated, but not the value.
#' @return \code{val_lab} return value labels (named vector). If labels doesn't
#'   exist it return NULL . \code{val_lab<-} return variable (vector x) which
#'   contains value labels.
#' @details Value labels are stored in attribute "labels"
#'   (\code{attr(x,"labels")}).
#' @export
#' @references
#' This is a modified version from `expss` package.
#' @examples
#' # toy example
#' set.seed(123)
#' # score - evaluation of tested product
#'
#' score = sample(-1:1,20,replace = TRUE)
#' var_lab(score) = "Evaluation of tested brand"
#' val_lab(score) = c("Dislike it" = -1,
#'                    "So-so" = 0,
#'                    "Like it" = 1
#'                    )
#'
#'

val_lab <- function(x) {
    UseMethod("val_lab", x)
}


#' @export
val_lab.default <- function(x) {
    attr(x, "labels", exact = TRUE)
}

#' @export
val_lab.data.frame <- function(x) {
  # we consider data.frame as multiple response question
  all_labs = lapply(x, val_lab)
  all_labs = all_labs[lengths(all_labs) > 0]
  if (length(all_labs) > 0)
    return(all_labs)
  else
    return(NULL)
}

#####################

#' @export
#' @rdname val_lab
`val_lab<-` <- function(x, value) {
  UseMethod("val_lab<-", x)
}

#' @export
`val_lab<-.default` <- function(x, value) {

  if (length(value) == 0) {
    attr(x, "labels") = NULL
    return(x)
  }

  if (is.factor(x)) {
    label = var_lab(x)
    x = as.character(x)
    if (!is.null(label))
      var_lab(x) = label

  }

  if(any(names(value) == ""))
    warning("'val_lab' - labels has one or more blank names.")

  if(is.null(names(value)))
    stop("'val_lab' - labels should be named vector.")
  if(anyDuplicated(value))
    stop("'val_lab' - duplicated values in labels: ",
         paste(value[duplicated(value)], collapse = " "))

  # value = sort(value)
  attr(x, "labels") = value
  x

}



#' @export
#' @rdname val_lab
has.labels <- function(x) {
    !is.null(attr(x, "labels", exact = TRUE))
}

######

#' @export
#' @rdname val_lab
unval <- function(x) {
  val_lab(x) <- NULL
  return(x)
}

##########################

#' Drop variable label and value labels
#'
#' \code{unlab} returns variable x without variable labels and value labels
#'
#' @param x Variable(s). Vector/data.frame/list.
#' @return \code{unlab} returns original variable x without variable label, value labels.
#' @seealso \code{\link{drop_lab}} \code{\link{unval}}
#' @export
#' @references
#' This is a modified version from `expss` package.
#' @examples
#' raw_var <- rep(1:2,5)
#' var_with_lab <- raw_var
#' var_lab(var_with_lab) <- "Income"
#' val_lab(var_with_lab) <- c("Low"=1,"High"=2)
#' identical(raw_var,unlab(var_with_lab)) # should be TRUE
#'
unlab <- function(x) {
    UseMethod("unlab", x)
}

#' @export
unlab.default <- function(x) {
  if (is.null(x))
    return(x)
  if (is.list(x)) {
    return(unlab.list(x))
  }
  if(has.label(x))
    attr(x, "label") = NULL
  if(has.labels(x))
    attr(x, "labels") = NULL
  x
}

#' @export
unlab.data.frame <- function(x) {
    for (each in seq_along(x))
        x[[each]] = unlab(x[[each]])
    x
}


##########################

#' Copy variable label and value labels
#'
#' \code{copy_lab} copy the variable label and value labels or returns \code{old_var} to
#' variable \code{new_var} and return \code{new_var}.
#'
#' @param new_var Variable to be copied to.
#' @param old_var Variable to be copied from.
#' @param strict Should the variables should be the same \code{mode}.
#' @return \code{copy_lab} returns \code{new_var} with same variable label and value labels
#' as \code{old_var}.
#' @export
#' @examples
#' var_with_lab <- rep(1:2,5)
#' var_lab(var_with_lab) <- "Income"
#' val_lab(var_with_lab) <- c("Low"=1,"High"=2)
#' var_nolab <- rep(1:2, 10)
#' var_ut <- copy_lab(var_nolab, var_with_lab)
#' @export
copy_lab <- function(new_var, old_var, strict = TRUE){

  if(mode(new_var) != mode(old_var) & strict)
    stop("Different data types provided")

  var_lab(new_var) <- var_lab(old_var)
  if(mode(new_var) == mode(new_var) & has.labels(old_var))
    val_lab(new_var) <- val_lab(old_var)

  new_var
}


#' Replace vector/matrix/data.frame values with corresponding value labels.
#'
#' \code{lab2val} replaces vector/matrix/data.frame values with
#' corresponding value labels. If there are no labels for some values they are
#' converted to characters in most cases. If there are no labels at all for
#' variable it remains unchanged.
#'
#' @param x vector/matrix/data.frame
#' @return Object of the same form as x but with value labels instead of values.
#'
#' @seealso \link{val_lab}, \link{var_lab}
#' @references
#' This is a modified version from `expss` package.
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(mpg) = NULL
#'                 val_lab(am) = c(" automatic" = 0, " manual" =  1)
#' })
#'
#' table(lab2val(mtcars$am))
#'
#' summary(lm(mpg ~ ., data = lab2val(mtcars[,c("mpg","am")])))
#' @export
lab2val <- function(x){
  UseMethod("lab2val", x)
}

#' @export
lab2val.default <- function(x){
  vallab = val_lab(x)
  if(is.null(vallab))
    return(x)
  res = names(vallab)[match(x,vallab,incomparables = NA)]
  res_na = is.na(res)
  if(any(res_na)) res[res_na] = x[res_na]
  var_lab(res) = var_lab(x)
  res

}

#' @export
lab2val.matrix <- function(x){
  res = lab2val.default(x)
  res = matrix(res, nrow = nrow(x), ncol = ncol(x))
  res
}

#' @export
lab2val.data.frame <- function(x){
  for (each in seq_along(x)){
    x[[each]] = lab2val(x[[each]])
  }
  x
}


