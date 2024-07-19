#' Create a Kaplan-Meier plot using ggplot2
#'
#' @param sfit a \code{\link[survival]{survfit}} object
#' @param table logical: Create a table graphic below the K-M plot, indicating at-risk numbers?
#' @param xlabs x-axis label
#' @param ylabs y-axis label
#' @param ystratalabs The strata labels. \code{Default = levels(summary(sfit)$strata)}
#' @param ystrataname The legend name. Default = "Strata"
#' @param timeby numeric: control the granularity along the time-axis
#' @param main plot title
#' @param pval logical: add the pvalue to the plot?
#' @param ... option parameters include xlims and ylims to set the axes' ranges, 
#' where defaults are derived from the data: both are vectors of length two giving the min and max.
#' @return a ggplot is made. if return=TRUE, then an arrangeGlob object
#' is returned
#' @author Abhijit Dasgupta with contributions by Gil Tomas
#' \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}
#' @export
#' @examples
#' \dontrun{
#' data(colon)
#'  fit <- survfit(Surv(time,status)~rx, data=colon)
#'  ggkm(fit, timeby=500)
#' }
km_ggplot <- function(sfit, table=TRUE, 
                      xlabs = "Time", ylabs="",
                      ystratalabs = NULL, ystrataname = NULL,
                      timeby = 100,
                      pval = FALSE,  ...) {
  
  require(ggplot2)
  require(survival)
  require(patchwork)
  ldots <- list(...)
  
  if(is.null(ystratalabs)) {
    ystratalabs <- as.character(levels(summary(sfit)$strata))
    ystratalabs <- gsub("^.*\\=","", ystratalabs)
  }
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(sfit$time), by = timeby)
  strata <- summary(sfit, censored = T)$strata
  if(is.null(strata)){strata <- factor(rep(1, length(sfit$time)))}
  
  sfit$surv <- sfit$surv #To calculate cumulative probability of event rather than survival probability
  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, strata =strata ,
                    upper = sfit$upper, lower = sfit$lower)
  levels(.df$strata) <- ystratalabs
  d <- length(levels(.df$strata))
  
  if( "xlims" %in% names(ldots)){xlims <- ldots$xlims
  }else{ xlims <- c(0, max(sfit$time))}
  if( "ylims" %in% names(ldots)){ylims <- ldots$ylims
  }else{ ylims <- c(0, 1)}
  
  p <- ggplot(.df, aes(time, surv, group = strata)) +
    geom_step(aes(colour=strata, linetype=strata) ) +
    scale_x_continuous(xlabs, breaks = times, limits =xlims ) +
    scale_y_continuous(ylabs, limits = ylims) +
    theme(legend.position = "top") +
    theme(plot.margin=unit(c(0.5, 1, 0.5, 0.1*m), "lines"),
          axis.title.y = element_text(vjust=0)
          )
    
  # add confidence intervals 
  # geom_stepribbon taken from https://github.com/adibender/pammtools/blob/master/R/ggplot-extensions.R
  # 19JUL2024
  
  p <- p + geom_stepribbon(aes(ymin=lower, ymax=upper, fill=strata), 
                                      alpha=0.25)+
    labs( colour="", fill="", linetype="") 

  #https://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
  #https://stackoverflow.com/questions/33967078/create-a-ggplot2-survival-curve-with-censored-table  
  #https://rpkgs.datanovia.com/survminer/index.html
  
  if(pval) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", signif(pval, 3)))
    p <- p + annotate("text", x = 0.9 * max(sfit$time), y = 0.1, label = pvaltxt)
  }
  
  

    ## Create table graphic to include at-risk & event numbers
    summary_object <- summary(sfit, times = times, extend = TRUE)
    strata <- summary_object$strata
    if(is.null(strata)){strata <- factor(rep(1, length(times)))}
    
    
    risk.data <- data.frame(strata =  strata,
                            time =  summary_object$time,
                            n.risk =  summary_object$n.risk,
                            n.event=  summary_object$n.event)
    #take cumsum of events by strata
    risk.data$n.event <- unlist( with(risk.data, tapply(n.event, strata, cumsum)))
    data.table <- ggplot(risk.data, 
                         aes(x = time, y = strata, colour=strata,
                             label = paste0( 
                               format(n.risk, nsmall = 0),"/",
                               format(n.event, nsmall = 0)
                                           )
                             )
                         ) +
      geom_text(size = 3.5) +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = ystratalabs) +
      scale_x_continuous("at risk / events", limits = xlims, position="bottom") 
    
    data.table <- data.table + theme(legend.position = "none")+
      xlab(NULL) + ylab(NULL)
    data.table <- data.table +
      # plot.margin reflects: top, right, bottom, and left margins
      theme(plot.margin = unit(c(0.5, 1, 0.5, 0.1*m), "lines"),
            axis.text.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.ticks = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
      )
  ### modify        
output <- list(top=p, bottom=data.table)
class(output) <- "km_ggplot"
output
}

print.km_ggplot <- function(x,...){
  print(
  x$top/x$bottom + patchwork::plot_layout(heights=c(3,1))
  )
  invisible(x)
}

plot.km_ggplot <-   print.km_ggplot

