#'Produces summary statistics on a variable broken down by arm, display a graphical equivalent.
#'
#'@inheritParams sumfig
#'@param total logical value to include an extra overall columns. Defaults to true
#'@param fig logical value on whether to print the figure. Defaults to true
#'
#'@return a data.frame containing summary statistics in character format,
#'ready to use with write_table(). Plus an attribute "fig" that contains a ggplot object
#'
#'
#'@seealso \code{\link{sumfig}} , \code{\link{write_table}}
#'
#'@import magrittr
#'@export


sumby <- function(variable,
                  arm,
                  label = NULL,
                  data  = parent.frame(),
                  total = TRUE,
                  fig   = TRUE){

  variable_name <- deparse(substitute(variable))

  mf       <- match.call()
  variable <- eval(mf$variable, envir = data)
  arm      <- eval(mf$arm, envir = data)

  fig_object <- sumfig(variable = variable, arm = arm, label = label)

  if(fig){
    print(fig_object)
  }


  if(total){
    variable      <- rep(variable, 2)
    arm_character <- c(as.character(arm), rep("Total", length(arm)))
    if(is.factor(arm)){
      arm_levels <- c(levels(arm), "Total")
      arm        <- factor(arm_character, levels = arm_levels)
    } else{
      arm <- arm_character
    }
  }

  variable.class = class(variable)
  # continuous variable summary statistics by arm
  if(variable.class == "numeric" || variable.class == "integer"){
    mu   = format(tapply(variable, arm, mean, na.rm = T), digits = 3, width = 2)
    sd   = format(tapply(variable, arm, sd, na.rm = T), digits = 3, width = 2)
    n    = tapply(variable, arm, function(x){sum(!is.na(x))})
    meds = tapply(variable, arm, stats::median, na.rm = T)
    mins = tapply(variable, arm, min, na.rm = T)
    maxs = tapply(variable, arm, max, na.rm = T)
    if(is.null(label)){
      variable = c(propercase(variable_name), "", "", "")
    } else{
      variable = c(label, "", "", "")
    }
    stats = c("n", "Mean (SD)", "Median", "Min, Max")
    value = rbind(n, paste0(mu," (", sd, ")"), meds, paste0(mins, ", ", maxs))
    ans   = data.frame(cbind(variable, stats, value), row.names = 1:4)
  }
  # categorical variable summary statistics by arm
  if(variable.class == "factor" || variable.class == "character"){
    tab   = table(variable, arm)
    dims  = dim(tab)
    nams  = dimnames(tab)
    tab   = as.data.frame(tab)
    total = matrix(rep(with(tab, tapply(Freq, arm, sum)), dims[1]), nrow = dims[1], byrow = T)
    tab   = stats::reshape(tab, direction = "wide", v.names = "Freq", timevar = "arm", idvar = "variable")
    if(is.null(label)){
      variable = c(propercase(variable_name), rep("", dims[1]-1))
    } else{
      variable = c(label, rep("", dims[1]-1))
    }
    stats = propercase(tab[, 1])
    tab   = tab[, -1]
    perc  = round(100 * tab / total, 1)

    X        = array(0, dim = c(dim(perc), 3))
    X[, , 1] = as.matrix(perc)
    X[, , 2] = as.matrix(tab)
    X[, , 3] = as.matrix(total)

    perpast = function(x){
      paste0(x[1], "% (", x[2], "/", x[3], ")")
    }
    value           = apply(X, c(1, 2), perpast)
    colnames(value) = nams[[2]]

    ans = data.frame(cbind(variable, stats, value), row.names = 1:dims[1])
  }

  names(ans)[1:2]  <- c("Variable", "Statistics")
  ans %<>%
    apply(., 2, as.character) %>%
    gsub("^0% ", "", .)       %>%
    gsub("^NaN% ", "", .)

  attr(ans, "fig") <- fig_object
  ans
}
