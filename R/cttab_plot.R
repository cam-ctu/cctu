#' Create summary plot for cctab function
#'
#' @inheritParams cttab
#'
#' @keywords internal
#' @importFrom stats aggregate as.formula
#' @importFrom gridExtra grid.arrange
#'
cctab_plot <- function(vars,
                       data,
                       group = NULL,
                       row_split = NULL,
                       select = NULL) {
  # Set missing to those will be excluded
  if (!is.null(select)) {
    for (i in names(select)) {
      sel <- gen_selec(data, i, select[i])
      data[[i]][!sel] <- NA
    }
  }

  vars <- unlist(vars, use.names = FALSE)

  logic_vars <- sapply(vars, function(x) is.logical(data[[x]]))

  # For numeric variables, count number of TRUE
  if (any(logic_vars)) {
    # Extract labels
    logis_labs <- sapply(vars[logic_vars], function(x) {
      ifelse(has_label(data[[x]]), var_lab(data[[x]]), x)
    })

    if (!is.null(group) || !is.null(row_split)) {
      data <- data.table::as.data.table(data)
      # replacing the setDT(data) in the line below
      logic_dt <- data[, lapply(.SD, sum, na.rm = TRUE),
        by = c(group, row_split),
        .SDcols = vars[logic_vars]
      ]

      logic_dt <- melt(logic_dt,
        id.vars = c(group, row_split),
        measure.vars = vars[logic_vars],
        variable.name = "logic_variables"
      )
    } else {
      logic_ctn <- sapply(vars[logic_vars], function(x) sum(data[[x]]))
      logic_dt <- data.frame(
        logic_variables = names(logic_ctn),
        value = logic_ctn,
        row.names = NULL
      )
    }

    logic_dt$logic_variables <- factor(logic_dt$logic_variables,
      levels = names(logis_labs),
      labels = unname(logis_labs)
    )

    vars <- c(vars[!logic_vars], "logic_variables")
  }

  # Extract group and split labels
  if (!is.null(group)) {
    gp_lab <- ifelse(has_label(data[[group]]), var_lab(data[[group]]), group)
  } else {
    gp_lab <- NULL
  }

  if (!is.null(row_split)) {
    rs_lab <- ifelse(has_label(data[[row_split]]),
      var_lab(data[[row_split]]),
      row_split
    )
  } else {
    rs_lab <- NULL
  }


  p_list <- lapply(vars, function(v) {
    v_lab <- ifelse(has_label(data[[v]]), var_lab(data[[v]]), v)

    # Convert character to factor
    if (v != "logic_variables" && (has_labels(data[[v]]) ||
                                     is.character(data[[v]]))) {
      data[[v]] <- to_factor(data[[v]], ordered = TRUE)
    }

    # Barplot for logical variable
    if (v == "logic_variables") {
      p <- ggplot(logic_dt, aes(
        x = .data[["logic_variables"]],
        y = .data[["value"]],
        fill = group
      )) +
        geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
        labs(x = "Logic variables (count TRUE)", y = "Count") +
        theme(legend.position = "top")
      if (!is.null(row_split)) {
        p <- p + facet_wrap(as.formula(paste("~", row_split)))
      }

      return(p)
    }

    # Barplot for character variable
    if (inherits(data[[v]], c("factor", "character"))) {
      if (is.null(group)) {
        p <- ggplot(data, aes(x = .data[[v]])) +
          geom_bar(position = "dodge", na.rm = TRUE) +
          labs(x = v_lab)

        return(p)
      }

      p <- ggplot(data, aes(x = .data[[group]], fill = .data[[v]])) +
        geom_bar(position = "dodge", na.rm = TRUE) +
        labs(x = gp_lab, fill = v_lab) +
        theme(legend.position = "top")

      if (!is.null(row_split)) {
        p <- p + facet_wrap(as.formula(paste("~", row_split)))
      }

      return(p)
    }

    # Boxplot for a numerical variable
    if (inherits(data[[v]], c("numeric", "integer"))) {
      if (is.null(group)) {
        if (is.null(row_split)) {
          p <- ggplot(data, aes(y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(y = v_lab) +
            theme(legend.position = "top")
        } else {
          p <- ggplot(data, aes(x = .data[[row_split]], y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(x = rs_lab, y = v_lab) +
            theme(legend.position = "top")
        }
      } else {
        if (is.null(row_split)) {
          p <- ggplot(data, aes(x = .data[[group]], y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(x = gp_lab, y = v_lab) +
            theme(legend.position = "top")
        } else {
          p <- ggplot(data, aes(
            x = .data[[row_split]],
            y = .data[[v]],
            fill = .data[[group]]
          )) +
            geom_boxplot(na.rm = TRUE) +
            labs(x = rs_lab, y = v_lab, fill = gp_lab) +
            theme(legend.position = "top")
        }
      }
      p
    }
  })

  # print
  # Plot every 8 plots
  p_len <- split(seq_along(p_list), ceiling(seq_along(p_list) / 8))
  for (i in p_len) {
    grid.arrange(
      grobs = p_list[i],
      ncol = 2,
      nrow = ceiling(length(p_list[i]) / 2)
    )
  }
}
