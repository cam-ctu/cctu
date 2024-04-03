

coef_table <- function(x,...){
  UseMethod("coef_table")
}

covar <- function(x,...){
  UseMethod("covar")
}


coef_table.lm <- function(x,level=0.95, ...){
  output <- cbind(
    summary(x)$coefficients[,c(1,2,4)],
    confint(x, level=level)
  )
  colnames(output) <- c("beta","se","p","lower","upper")
  output
}

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






regression_table <- function(x,labels=names(coef(x)),
                             digits=3,p_digits=4,
                             trans=if(class(x)[1]=="lm"){NULL}else{exp},
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
  p <-  format_pval( coef[,"p"], digits=p_digits)
  # this works with est_trans as either null (ignores it), or a vector
  var_list <- list(labels, est,est_trans, ci,p)
  X <- data.frame(do.call(cbind, var_list))
  # Needs to be more general to deal with extra transformed columns
  empty <- list("")
  Y <- as.data.frame(
    do.call( cbind,
             c(list(covar(x, digits=digits)),
               empty[rep(1, ncol(X)-2)])
    )
  )
  names(Y) <- names(X)
  X <- rbind_space(X, Y)
  names(X) <- col_names
  X
}


guess_col_names <- function(x,trans,...){
  UseMethod("guess_col_names")
}

guess_col_names.default <- function(x,trans,...){
  c("Parameter", "Estimate (SE)" ,  "Conf. Int." ,"p-value")
}

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

guess_col_names.coxph <- function(x,trans,...){
  c("Parameter", "Log HR (SE)" ,  "HR","Conf. Int." ,"p-value")
}





library(MASS)
data(cats)
fit_lm <- lm(Hwt~Sex/Bwt-1, data=cats)


regression_table(fit_lm,
                 labels=c("Female","Male", "Female - Bwt","Male - Bwt")

                  )


data("birthwt")
head(birthwt)
fit_glm <- glm(low~. -bwt , family=gaussian, data=birthwt)
regression_table(fit_glm, trans=NULL)

