#' Create a Kaplan-Meier plot using ggplot2
#'
#' @param sfit a \code{\link[survival]{survfit}} object
#' @param xlabs x-axis label
#' @param ylabs y-axis label
#' @param timeby numeric: Default is NULL to use ggplot defaults, but allows user to specify the gaps between x-axis ticks
#' @param strata_labs The strata labels. If left as NULL it defaults to \code{levels(summary(sfit)$strata)} with minor prettification.
#' @param ystratalabs deprecated and only for back compatibility. use strata_labs argument.
#' @param pval logical: add the p-value to the plot?
#' @param  p_digits integer: the number of decimal places to use for a p-value.
#' @param ... option parameters include `xlims` and `ylims` to set the axes' ranges,
#' where defaults are derived from the data: both are vectors of length two giving the min and max.
#' @return a list of ggplot objects is made: the top figure and a table of counts.
#' The object has a print and plot method that uses  \code{\link[patchwork]{wrap_plots}}
#'  to glue together, internal function \code{\link{build_kmplot.}} The user can access and modify the ggplot components as desired.
#'
#' @details
#' This function will return a list of `ggplot2` object. The KM-plot will stored
#' at `top` and risktable will stored at `bottom`. You can modifies those as you
#' normally draw a plot with `ggplot2`. You can modify anything you want except
#' the x-axis scale of the plot, otherwise the x-axis of KM-plot and the risk
#' table will not align. There are other packages, like `ggsurvfit`,
#' you can use to draw a KM-plot with more options.
#'
#' @author Original taken from  \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/} but modified by authors of \code{cctu} package.
#' @export
#' @importFrom ggplot2 ggplot aes geom_step scale_y_continuous scale_x_continuous theme  element_text layer_scales  labs xlab ylab unit element_blank geom_text annotate
#' @importFrom survival survdiff
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @examples
#' library(survival)
#'  fit <- survfit(Surv(time,status)~rx, data=colon)
#'  km_ggplot(fit)
#'  ## Change theme of the KM-plot
#'  p <- km_ggplot(fit)
#'  p$top <- p$top +
#'     theme_classic()
#'  # Change the theme of the risktable
#'  p$bottom <- p$bottom +
#'     theme_void()
#'
#'  plot(p)
#'

km_ggplot <- function(sfit,
                      xlabs = "Time", ylabs="",
                      strata_labs=NULL,
                      ystratalabs = deprecated(),
                      timeby = NULL,
                      pval = FALSE,
                      p_digits=getOption("cctu_p_digits", default = 4),
                      ...) {
  if (is_present(ystratalabs)) {
    deprecate_warn("0.8.4", "km_ggplot(ystratalabs)", "km_ggplot(strata_labs)")
    strata_labs <- ystratalabs
  }

  ldots <- list(...)
  strata <- summary(sfit, censored = T)$strata



  if(is.null(strata_labs)) {
    strata_labs <- if( is.null( strata)){ "Pooled"} else{as.character(levels(summary(sfit)$strata))}
    # count up the number of `=` in the labs, If >1 then leave alone.
    if(  1 == min( lengths(regmatches(strata_labs, gregexpr('=', strata_labs))))){
      strata_labs <- gsub("^.*\\=","", strata_labs)
    }
  }
  m <- max(nchar(strata_labs))
  times <- if(!is.null(timeby)){ seq(0, max(sfit$time), by = timeby) } else { waiver() }

  if(is.null(strata)){strata <- factor(rep(1, length(sfit$time)))}

  sfit$surv <- sfit$surv #To calculate cumulative probability of event rather than survival probability
  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, strata =strata ,
                    upper = sfit$upper, lower = sfit$lower)
  levels(.df$strata) <- strata_labs


  if( "xlims" %in% names(ldots)){xlims <- ldots$xlims
  }else{ xlims <- c(0, max(sfit$time))}
  if( "ylims" %in% names(ldots)){ylims <- ldots$ylims
  }else{ ylims <- c(0, 1)}

  p <- ggplot(.df, aes(x=.data$time, y=.data$surv, group = .data$strata)) +
    geom_step(aes(colour=.data$strata, linetype=.data$strata) ) +
    scale_x_continuous(xlabs, breaks = times, limits =xlims ) +
    scale_y_continuous(ylabs, limits = ylims)

  if( 1< nlevels(.df$strata)){
    p <- p+ theme(legend.position = "top") +
      theme(
        plot.margin=unit(c(0.5, 1, 0.5, 0.1*m), "lines"),
        axis.title.y = element_text(vjust=0)
      )
  }else{
    p <- p+theme(legend.position = "none")

  }


  # add confidence intervals
  # geom_stepribbon taken from https://github.com/adibender/pammtools/blob/master/R/ggplot-extensions.R
  # 19JUL2024

  p <- p + geom_stepribbon(aes(ymin=.data$lower, ymax=.data$upper, fill=.data$strata),
                                      alpha=0.25)+
    labs( colour="", fill="", linetype="")

  #https://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
  #https://stackoverflow.com/questions/33967078/create-a-ggplot2-survival-curve-with-censored-table
  #https://rpkgs.datanovia.com/survminer/index.html

  if(pval) {
    sdiff <- survival::survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- sdiff$pvalue
    pvaltxt <-   format_pval(pval, digits=p_digits) # There are two p-value formats
    if( substr(pvaltxt,1,1)=="<"){ sep=" "}else{ sep=" ="}
    pvaltxt <- paste0("p",sep, pvaltxt)
    x_pos <- layer_scales(p)$x$get_limits() %*% c(0.9,0.1)
    y_pos <- layer_scales(p)$y$get_limits() %*% c(0.9,0.1)
    p <- p + annotate("text", x = x_pos, y =y_pos, label = pvaltxt)
  }



    ## Create table graphic to include at-risk & event numbers
  times <- layer_scales(p)$x$get_breaks()

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
                         aes(x = .data$time, y = .data$strata, colour=.data$strata,
                             label = paste0(
                               format(.data$n.risk, nsmall = 0),"/",
                               format(.data$n.event, nsmall = 0)
                                           )
                             )
                         ) +
      geom_text(size = 3.5) +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = strata_labs) +
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

    attr(output, "nstrata") <- length(unique(strata))

    return(output)
}


#' print methods for km_ggplot object
#'
#' @param x km_ggplot object
#' @param ... other arguments for generic methods
#' @export
print.km_ggplot <- function(x,...){
  plot(build_kmplot(x))
  invisible(x)
}

#' plot methods for km_ggplot object
#'
#' @param x km_ggplot object
#' @param ... other arguments for generic methods


#' @export
plot.km_ggplot <-   print.km_ggplot


#' Combine KM-plot with risk table
#'
#' @param x km_ggplot object
#' @param ... other arguments not used
#' @importFrom patchwork wrap_plots
#' @keywords internal
build_kmplot <- function(x, ...){

  stopifnot(inherits(x, "km_ggplot"))

  nstrata <- attr(x, "nstrata")
  tbl_height <- 0.03067 + 0.03466 * nstrata

  p_combined <- wrap_plots(x$top, x$bottom,
                           ncol = 1,
                           heights = c(1 - tbl_height, tbl_height))

  return(p_combined)
}
