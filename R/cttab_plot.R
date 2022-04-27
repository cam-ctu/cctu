

#' Create summary plot for cctab function
#'
#' @inheritParams cttab
#'
#' @keywords internal
#' @importFrom stats aggregate as.formula
#'
cctab_plot <- function(vars,
                       data,
                       group = NULL,
                       row_split = NULL,
                       select = NULL){

  # Set missing to those will be excluded
  if(!is.null(select)){
    for(i in names(select)){
      sel <- gen_selec(data, i, select[i])
      data[[i]][!sel] <- NA
    }
  }

  vars <- unlist(vars, use.names = FALSE)

  logic_vars <- sapply(vars, function(x)is.logical(data[[x]]))

  # For numeric variables, count number of TRUE
  if(any(logic_vars)){

    # Extract labels
    logis_labs <- sapply(vars[logic_vars], function(x){
      ifelse(has.label(data[[x]]), var_lab(data[[x]]), x)
    })

    if(!is.null(group) | !is.null(row_split)){
      fom_ag <- paste("cbind(", paste(vars[logic_vars], collapse = ","),
                      ") ~ ", paste(c(group, row_split), collapse = "+"))

      logic_dt <- aggregate(as.formula(fom_ag), data = data,
                            sum)
      setDT(logic_dt)

      logic_dt <- melt(logic_dt, id.vars = c(group, row_split),
                       measure.vars = vars[logic_vars],
                       variable.name = "logic_variables")

    }else{
      logic_ctn <- sapply(vars[logic_vars], function(x)sum(data[[x]]))
      logic_dt <- data.frame(logic_variables = names(logic_ctn),
                             value = logic_ctn,
                             row.names = NULL)
    }

    logic_dt$logic_variables <- factor(logic_dt$logic_variables,
                                       levels = names(logis_labs),
                                       labels = unname(logis_labs))

    vars <- c(vars[!logic_vars], "logic_variables")
  }

  # Extract group and split labels
  if(!is.null(group))
    gp_lab <- ifelse(has.label(data[[group]]), var_lab(data[[group]]), group)
  else
    gp_lab <- NULL

  if(!is.null(row_split))
    rs_lab <- ifelse(has.label(data[[row_split]]), var_lab(data[[row_split]]), row_split)
  else
    rs_lab <- NULL


  p_list <- lapply(vars, function(v){

    v_lab <- ifelse(has.label(data[[v]]), var_lab(data[[v]]), v)

    # Convert character to factor
    if(v != "logic_variables" && (has.labels(data[[v]]) || is.character(data[[v]])))
      data[[v]] <- to_factor(data[[v]], ordered = TRUE)

    # Barplot for logical variable
    if(v == "logic_variables"){
      p <- ggplot(logic_dt, aes_string(x = "logic_variables", y = "value", fill = group)) +
        geom_bar(stat = "identity", position = "dodge", na.rm=TRUE) +
        labs(x = "Logic variables (count TRUE)", y = "Count")
      if(!is.null(row_split))
        p <- p + facet_wrap(as.formula(paste("~", row_split)))

      return(p)
    }

    # Barplot for logical variable
    if(inherits(data[[v]], c("factor", "character"))){
      p <- ggplot(data, aes_string(x = v, fill = group)) +
        geom_bar(position = "dodge", na.rm=TRUE) +
        labs(x = v_lab, fill = gp_lab)
      if(!is.null(row_split))
        p <- p + facet_wrap(as.formula(paste("~", row_split)))

      return(p)
    }

    # Boxplot for a numerical variable
    if(inherits(data[[v]], c("numeric", "integer"))){
      if(is.null(row_split))
        p <- ggplot(data, aes_string(x = group, y = v, fill = group)) +
          geom_boxplot(na.rm=TRUE) +
          labs(x = gp_lab, y = v_lab, fill = gp_lab)
      else
        p <- ggplot(data, aes_string(x = row_split, y = v, fill = group)) +
          geom_boxplot(na.rm=TRUE) +
          labs(x = rs_lab, y = v_lab, fill = gp_lab)

      return(p)
    }

  })

  # Plot every 9 plots
  p_len <- split(seq_along(p_list), ceiling(seq_along(p_list)/9))
  com_leg <- !is.null(group)
  for(i in p_len){
    p <- grid_arrange_shared_legend(plot_list = p_list[i], ncol = 3,
                                    common_leg = com_leg)
    grid.newpage()
    grid.draw(p)
  }

}


#' Arrange multiple plots with shared legend
#'
#'
#' @param plot_list A list of ggplot list.
#' @param ncol Number of columns
#' @param nrow Number of rows
#' @param position Position of legend
#' @param common_leg If common legend
#' @keywords internal
#' @references https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gridExtra)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' p1 <- qplot(carat, price, data = dsamp, colour = clarity)
#' p2 <- qplot(cut, price, data = dsamp, colour = clarity)
#' p3 <- qplot(color, price, data = dsamp, colour = clarity)
#' p4 <- qplot(depth, price, data = dsamp, colour = clarity)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
#' }
#' @importFrom grid unit.c grid.newpage grid.draw
#' @importFrom gridExtra arrangeGrob grid.arrange
grid_arrange_shared_legend <- function(plot_list,
                                       ncol = 1,
                                       nrow = ceiling(length(plot_list)/ncol),
                                       position = c("bottom", "right"),
                                       common_leg = TRUE) {

  position <- match.arg(position)

  if(common_leg){
    g <- ggplotGrob(plot_list[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plot_list, function(x) x + theme(legend.position="none"))
  }else{
    gl <- plot_list
  }

  gl <- c(gl, ncol = ncol, nrow = nrow)

  if(common_leg){
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  }else{
    combined <- do.call(arrangeGrob, gl)
  }

  # return gtable invisibly
  invisible(combined)

}

