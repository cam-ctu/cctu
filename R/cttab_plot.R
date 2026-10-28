#' Create summary plot for cttab function
#'
#' @inheritParams cttab
#'
#' @keywords internal
#' @importFrom stats as.formula
#' @importFrom gridExtra grid.arrange
#'
cttab_plot <- function(vars,
                       data,
                       group = NULL,
                       row_split = NULL,
                       select = NULL) {
  vars <- unlist(vars, use.names = FALSE)
  if (length(vars) == 0L) return(invisible(NULL))

  data <- data.table::copy(data.table::as.data.table(data))

  # Render labelled-numeric group / row_split as discrete categories
  # (avoids ggplot continuous-x / dropped-fill warnings).
  cttab_factorise(data, c(group, row_split))

  # Apply per-variable `select` filters by NA-ing out the excluded rows;
  # subsequent `na.rm = TRUE` in geoms then drops them from the plot.
  #
  # Evaluate every mask against the untouched data BEFORE applying any of them.
  # Interleaving the two let one variable's freshly-written NAs feed the next
  # variable's filter: with select = c(RACEN = "AGE > 60", BMIBL = "RACEN != 1")
  # NA-ing RACEN first turns "RACEN != 1" into NA (which counts as excluded),
  # so the plot showed fewer points than the table and the result depended on
  # the order of the names in `select`. stat_tab() evaluates each mask
  # independently against an unmutated `.SD`; the plot has to agree with it.
  if (!is.null(select)) {
    sel_vars <- intersect(names(select), vars)
    keeps <- lapply(sel_vars, function(v) cttab_eval_select(data, v, select))
    names(keeps) <- sel_vars
    for (v in sel_vars) {
      data.table::set(data, i = which(!keeps[[v]]), j = v, value = NA)
    }
  }

  logic_vars <- vapply(vars, function(x) is.logical(data[[x]]), logical(1L))

  logic_dt <- NULL
  if (any(logic_vars)) {
    logic_names <- vars[logic_vars]
    logis_labs <- vapply(logic_names,
                         function(x) cttab_get_label(data[[x]], x),
                         character(1L))

    if (!is.null(group) || !is.null(row_split)) {
      logic_dt <- data[, lapply(.SD, sum, na.rm = TRUE),
        by = c(group, row_split),
        .SDcols = logic_names
      ]
      logic_dt <- data.table::melt(
        logic_dt,
        id.vars = c(group, row_split),
        measure.vars = logic_names,
        variable.name = "logic_variables"
      )
    } else {
      logic_dt <- data.table::data.table(
        logic_variables = logic_names,
        value = vapply(logic_names,
                       function(x) sum(data[[x]], na.rm = TRUE),
                       numeric(1L))
      )
    }

    data.table::set(
      logic_dt,
      j = "logic_variables",
      value = factor(logic_dt[["logic_variables"]],
                     levels = logic_names,
                     labels = unname(logis_labs))
    )

    vars <- c(vars[!logic_vars], "logic_variables")
  }

  # Axis / legend labels for group and row_split
  gp_lab <- if (!is.null(group)) cttab_get_label(data[[group]], group) else NULL
  rs_lab <- if (!is.null(row_split)) {
    cttab_get_label(data[[row_split]], row_split)
  } else {
    NULL
  }

  p_list <- lapply(vars, function(v) {
    # Logical-variable barplot uses the pre-aggregated logic_dt
    if (v == "logic_variables") {
      p <- ggplot(logic_dt, aes(
        x = .data[["logic_variables"]],
        y = .data[["value"]]
      )) +
        labs(x = "Logic variables (count TRUE)", y = "Count")
      if (!is.null(group)) {
        p <- p + geom_bar(aes(fill = .data[[group]]),
                          stat = "identity", position = "dodge",
                          na.rm = TRUE) +
          labs(fill = gp_lab) +
          theme(legend.position = "top")
      } else {
        p <- p + geom_bar(stat = "identity", position = "dodge",
                          na.rm = TRUE)
      }
      if (!is.null(row_split)) {
        p <- p + facet_wrap(as.formula(paste("~", row_split)))
      }
      return(p)
    }

    # Nothing to plot if every observation is missing
    if (all(is.na(data[[v]]))) return(NULL)

    v_lab <- cttab_get_label(data[[v]], v)

    if (has_labels(data[[v]]) || is.character(data[[v]])) {
      data[[v]] <- to_factor(data[[v]], ordered = TRUE)
    }

    # Categorical: barplot
    if (inherits(data[[v]], c("factor", "character"))) {
      if (is.null(group)) {
        return(
          ggplot(data, aes(x = .data[[v]])) +
            geom_bar(position = "dodge", na.rm = TRUE) +
            labs(x = v_lab)
        )
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

    # Numeric: boxplot
    if (inherits(data[[v]], c("numeric", "integer"))) {
      if (is.null(group) && is.null(row_split)) {
        return(
          ggplot(data, aes(y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(y = v_lab)
        )
      }
      if (is.null(group)) {
        return(
          ggplot(data, aes(x = .data[[row_split]], y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(x = rs_lab, y = v_lab)
        )
      }
      if (is.null(row_split)) {
        return(
          ggplot(data, aes(x = .data[[group]], y = .data[[v]])) +
            geom_boxplot(na.rm = TRUE) +
            labs(x = gp_lab, y = v_lab) +
            theme(legend.position = "top")
        )
      }
      return(
        ggplot(data, aes(x = .data[[row_split]],
                         y = .data[[v]],
                         fill = .data[[group]])) +
          geom_boxplot(na.rm = TRUE) +
          labs(x = rs_lab, y = v_lab, fill = gp_lab) +
          theme(legend.position = "top")
      )
    }

    NULL
  })

  p_list <- p_list[!vapply(p_list, is.null, logical(1L))]
  if (length(p_list) == 0L) return(invisible(NULL))

  # Print at most 8 plots per page (2 columns)
  chunks <- split(seq_along(p_list), ceiling(seq_along(p_list) / 8))
  for (ix in chunks) {
    grid.arrange(
      grobs = p_list[ix],
      ncol = 2,
      nrow = ceiling(length(ix) / 2)
    )
  }

  # Returned (invisibly) so the filtered plot data can be asserted on; the
  # function is called for its side effect of drawing.
  invisible(p_list)
}
