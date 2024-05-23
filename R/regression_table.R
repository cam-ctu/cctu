#' Produce a table summarising a regression model for a study report
#'
#' @param x a regression object
#' @param labels character vector describing the meaning of the coefficient parameters in plain English
#' @param digits integer giving the number of significant figures to print
#' @param p_digits integer giving the number of digits to print p-values, or print as "<0.001" for example
#' @param trans a function to transform the coefficients by, e.g. present the odds ratios,
#' as well as log-odds ratios. It intelligent tries to guess between no transformation and exp, but may be wrong.
#' @param level value in the unit interval to use for calculating confidence intervals
#' @param col_names character vector of the column labels.
#' It intelligently tries to guess based on the class of x and the transformation,
#' but may be wrong.
#' @returns a matrix giving standard inference of coefficients, SE, confidence intervals, p-values,
#' plus a brief summary of the number of data points and residual error variance.
#'
#' @details methods exists when x is of the following classes: \code{lm, glm, gls, lme, coxph, gee}.
#' Extensions to other classes may be written by defining methods for \code{coef_table} and \code{covar}
#' functions
#' @examples
#' library(survival)
#' cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)
#' regression_table(cfit1, digits=4,
#' labels=c("Age (per year)","Sex (Female vs Male)","Weight loss (per pound)"))
#'
#' @importFrom stats confint pnorm qnorm
#' @export





regression_table <- function(x,labels=names(coef(x)),
                             digits=3,p_digits=digits,
                             trans=if(class(x)[1] %in% c("glm","coxph")){exp}else{NULL},
                             level=0.95,
                             col_names=guess_col_names(x,trans)
){
  coef <- coef_table(x, level=level)
  est <- paste0( signif_pad(coef[,"beta"],digits=digits),
                 " (", signif_pad(coef[,"se"],digits=digits),")")
  est_trans <- NULL
  if(!is.null(trans)){
    est_trans <- signif_pad( trans(coef[,"beta"]), digits=digits)
    coef[,"lower"] <- trans(coef[,"lower"])
    coef[,"upper"] <- trans(coef[,"upper"])
  }

  ci <-   paste0( signif_pad(coef[,"lower"],digits=digits), ", ",
                  signif_pad(coef[,"upper"],digits=digits))
  p <-  format_pval( coef[,"p"], digits=p_digits, sig.limit=10^(-p_digits))
  # this works with est_trans as either null (ignores it), or a vector
  var_list <- list(labels, est,est_trans, ci,p)
  X <- data.frame(do.call(cbind, var_list))
  # Needs to be more general to deal with extra transformed columns
  empty <- list("")
  Z <- covar(x, digits=digits)
  Y <- as.data.frame(
    do.call( cbind,
             c(list(Z),
               empty[rep(1, ncol(X)-ncol(Z))])
    )
  )
  names(Y) <- names(X)
  X <- rbind_space(X, Y)
  names(X) <- col_names
  if( inherits(x,c("gls","lme","gee"))){warning(
    "The variance-correlation structure is not included as it is too general
  consider summary(x), coef(x$modelStruct$corStruct, uncons=FALSE),
  or coef(x$modelStruct$varStruct, uncons=FALSE)")
  }
  X
}


# This set of generic & methods produce the initial part of the table

coef_table <- function(x,...){
  UseMethod("coef_table")
}



#' @export
coef_table.lm <- function(x,level=0.95, ...){
  output <- cbind(
    summary(x)$coefficients[,c(1,2,4), drop=FALSE],
    confint(x, level=level)
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

#' @export
coef_table.coxph <- function(x,level=0.95, ...){
  output <- cbind(
    summary(x)$coefficients[,c(1,3,5), drop=FALSE],
    confint(x, level=level)
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

#' @export
coef_table.lme <- function(x,level=0.95, ...){
  output <- cbind(
    summary(x)$tTable[,c(1,2,5), drop=FALSE],
    nlme::intervals(x, level=level,which="fixed")$fixed[,c(1,3)]
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

#' @export
coef_table.gee <- function(x, level=0.95,...){
  tab <- summary(x)$coefficient
  est <- tab[,1]
  se <- tab[,4]
  z <- tab[,5]
  p <- pnorm(-abs(z))*2
  k <- qnorm((1+level)/2)
  lower <- est-k*se
  upper <- est+k*se

  output <- cbind(
    est,se, p, lower, upper
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

#' @export
coef_table.gls <- function(x, level=0.95,...){
  output <- cbind(
    summary(x)$tTable[,c(1,2,4), drop=FALSE],
    nlme::intervals(x, level=level, which="coef")$coef[,c(1,3)]
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

# This set of generic/methods produce the covariance/nobs description


covar <- function(x,...){
  UseMethod("covar")
}

#' @export
covar.lm <- function(x,digits=3,...){
  x_summary <- summary(x)
  # covariance  do the formating here
  sigma <- x_summary$sigma
  n <- sum(x_summary$df[1:2])
  output <- cbind(
    contrast=c("Residual SE", "Number of Observations"),
    beta=c(signif_pad(sigma, digits=digits),n)
  )
  output
}

#' @export
covar.glm <- function(x,digits=3,...){
  x_summary <- summary(x)
  # covariance  do the formating here
  sigma <- x_summary$dispersion
  n <- sum(x_summary$df[1:2])
  output <- cbind(
    contrast=c("Dispersion", "Number of Observations"),
    beta=c(signif_pad(sigma, digits=digits),n)
  )
  output
}

#' @export
covar.coxph <- function(x,...){
  output <- cbind(
    contrasts=c("Number of Observations","Number of Events"),
    beta=c(x$n, x$nevent)
    )
  output
  }

#' @export
covar.lme <- function(x, digits=3,...){
    X <- nlme::VarCorr(x)
    X <- rbind(colnames(X), X)
    # Want to apply formatting to numbers
    index <- grep("\\d+\\.\\d+", X)
    X[index] <- X[index] %>% as.numeric %>% signif_pad(digits=digits)

    n <- x$dims$N
    n_groups <- x$dims$ngrps[1:x$dims$Q]
    contrasts <- c("Random Effects", rownames(X)[-1],"Number of Observations",names(n_groups))
    cols <- ncol(X)
    empty <- list("")
    Y <- do.call(cbind, c(list(c(n,n_groups)), empty[rep(1,cols-1)]))
    output <- cbind(contrasts, rbind(X,Y))
    output
  }

#' @export
covar.gls <- function(x, digits=3, ...){
  data.frame(contrasts=c("Number of Observations", "Number of Groups","Residual SE"),
             beta=c(x$dims$N,
                    nlevels(nlme::getGroups(x)),
                    signif_pad(x$sigma, digits=digits))
  )
}

#' @export
covar.gee <- function(x, digits=3,...){
  data.frame(contrasts=c("Number of Observations", "Number of Groups","Residual SE"),
             beta=c(x$nobs,
                    length(unique(x$id)),
                    signif_pad(sqrt(x$scale), digits=digits))
  )

}


# set of functions to guess column names. Has caveats

guess_col_names <- function(x,trans,...){
  UseMethod("guess_col_names")
}

#' @export
guess_col_names.default <- function(x,trans,...){
  c("Parameter", "Estimate (SE)" ,  "Conf. Int." ,"p-value")
}

#' @export
guess_col_names.glm <- function(x,trans,...){
  if( is.null(trans)){
    NextMethod()
  } else{
    family=x$family$family
    middle <-switch(
      family,
      binomial=c("Log OR (SE)", "OR"),
      poisson=c("Log RR (SE)", "RR"),
      c("Estimate (SE)", "Exp Est")
      )
  c("Parameter", middle,  "Conf. Int." ,"p-value")
  }
}

#' @export
guess_col_names.coxph <- function(x,trans,...){
  c("Parameter", "Log HR (SE)" ,  "HR","Conf. Int." ,"p-value")
}

#' @export
guess_col_names.gls <- function(x,trans,...){
  if( is.null(trans)){
    NextMethod()
  } else{
    c("Parameter", "Log Est (SE)" ,  "Ratio","Ratio C.I." ,"p-value")
  }
}








