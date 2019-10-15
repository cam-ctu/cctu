#' Produce ggplot to represent summary statistics graphically, either boxplot or barchart
#'
#' @param variable A variable of interest to be summarised. Works for several classes: numeric, integer, factor, character.
#' @param arm A variable to break down the summary by, typically an arm in a RCT. Or a constant variable for 1-armed study
#' @param label a text string to label the variable by, used to add in units or full words with spaces rather than acronyms. Defaults to NULL.
#' @param data optional a data frame, or envirnoment, in which to find the variable and arm values. Defaults to parent.frame()
#' @param ... list of other values to input into the aes() function
#' @return a ggplot object, either a boxplot or barchart
#'
#' @import ggplot2
#' @export



sumfig <- function(variable,
                   arm,
                   label = NULL,
                   data  = parent.frame(),
                   ...
                   ){
  variable_name <- deparse(substitute(variable))
  if(is.null(label)){label <- variable_name}

  mf       <- match.call()
  variable <- eval(mf$variable, envir = data)
  arm      <- eval(mf$arm, envir = data)
  data     <- data.frame(variable = variable, arm = arm)


  # produce boxplot by arm for continuous variables
  if(inherits(variable,c("numeric","integer"))){
    fig <- ggplot(data = data, aes(x = arm, y = variable), ... = ...) +
           geom_boxplot() +
           theme(axis.title.x = element_blank()) +
           labs(y = label)
  }

  # produce bar chart by arm for categorical variables
  if(inherits(variable, c("factor","character"))){
     fig <- ggplot(data = data, aes(x = arm, fill = variable), ... = ...) +
            geom_bar() +
            theme(axis.title.x = element_blank()) +
            labs(fill = label)
  }

  fig
}
