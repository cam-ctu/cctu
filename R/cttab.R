#' Generate an table of descriptive statistics.
#'
#' This is a wrapper function of \code{stat_tab}, allowing for grouped variables,
#' split statistics table by `row_split` variable.
#'
#' @param x Variables to be used or a \code{formula} for summary table.
#' If \code{x} is a \code{formula}, then the \code{group} variable should
#'  be provided at the right had side, use \code{1} if there's no grouping
#' variable. And \code{row_split} should also be provided on the right hand side
#' of the formula and separate it using \code{|} with grouping variable. For example,
#' \code{age + sex ~ treat|cycle} or \code{age + sex ~ 1|cycle} without grouping.
#' See details.
#' @param data A \code{data.frame} from which the variables in \code{vars}
#' should be taken.
#' @param group Name of the grouping variable.
#' @param row_split Variable that used for splitting table rows, rows will be
#'  split using this variable. Useful for repeated measures.
#' @param total If a "Total" column will be created (default). Specify
#' \code{FALSE} to omit the column.
#' @param select a named vector with as many components as row-variables. Every
#' element of `select` will be used to select the individuals to be analyzed
#'  for every row-variable. Name of the vector corresponds to the row variable,
#'  element is the selection.
#' @param add_missing If missing number and missing percentage will be
#'   reported in the summary table, default is `TRUE`. This will also produce
#' data missingness report if set \code{TRUE}. See \code{\link{report_missing}}
#' for details.
#' @param add_obs Add an observation row (default).
#' @param digits An integer specifying the number of significant digits to keep,
#' default is 3.
#' @param digits_pct An integer specifying the number of digits after the
#' decimal place for percentages, default is 0.
#' @param rounding_fn The function to use to do the rounding. Defaults is
#' \code{\link{signif_pad}}. To round up by digits instead of significant
#' values, set it to \code{round_pad}.
#' @param subjid_string A character naming the column used to identify subject,
#' default is \code{"subjid"}.
#' @param print_plot A logical value, print summary plot of the variables (default).
#' @param render_num A character or vector indicating which summary will be reported,
#'  default is "Median [Min, Max]". You can change this to "Median [Q1, Q3]" then the
#' median and IQR will be reported instead of "Median [Min, Max]". Use
#' \code{options(cctu_render_num = "Median [IQR]")} to set global options.
#' See details \code{\link{render_numeric}} \code{\link{num_stat}}.
#' @param logical_na_impute Impute missing values with \code{FALSE} (default),
#' \code{NA} keep as it is, or \code{TRUE}. The nominator for the logical vector is
#' the number of \code{TRUE}. For \code{FALSE} or \code{TRUE}, the denominator will
#'  be all values regardless of missingness, but the non-missing number used as
#' denominator for \code{NA}. Set it to \code{FALSE} if you want to summarise multiple
#' choice variables and \code{NA} for Yes/No type logical variables but don't want No
#' in the summary. You can used a named list in \code{x} and stack multiple
#'  choice in one category.
#' @param blinded A logical scalar, if summary table will be report by
#' \code{group} (default) or not. This will ignore \code{group} if set to \code{TRUE}
#' and grouping summary will not be reported.
#' @param ... Not used.
#' @details
#' \strong{1. Parameter settings with global options}
#'
#' Some of the function parameters can be set with options. This will have an global
#' effect on the \code{cctab} function. It is an ideal way to set a global settings
#' if you want this to be effective globally. Currently, you can set \code{digits},
#' \code{digits_pct}, \code{subjid_string}, \code{print_plot}, \code{render_num} and
#' \code{blinded}  by adding \code{"cctu_"} prefix in the \code{options}. For example,
#'  you can suppress the plot from printing by setting \code{options(cctu_print_plot = FALSE)}.
#'
#' \strong{2. Formula interface}
#'
#' There are two interfaces, the default, which typically takes a variable vector from
#' \code{data.frame} for \code{x}, and the formula interface. The formula interface is
#'  less flexible, but simpler to use and designed to handle the most common use cases.
#' For the formula version, the formula is expected to be a two-sided formula. Left hand
#' side is the variables to be summarised and the right hand side is the group and/or split
#'  variable. To include a row splitting variable, use \code{|} to separate the row splitting
#' variable after the grouping variable and then the row split variable. For example,
#' \code{age + sex ~ treat|visit}. The right hand side of the formula will be treated as a grouping
#'  variable by default. A value of \code{1} should be provided if there is no grouping variable,
#'  for example \code{age + sex ~ 1} or \code{age + sex ~ 1|visit} by visit.
#'
#' \strong{3. Return}
#'
#' A summary table with some attributes will be reutned, a method has been writen for \code{rbind}.
#' So you can use \code{rbind} to combine two tables without losing any attributes. An attribute
#' \code{position} will be used to produce a nice table. There are three 4 possible values for each
#' rows. Row name printed as the first column in the word table. Some styles will be applied to each
#' row based on the \code{position} attributes.
#' \tabular{ll}{
#' \code{0} \tab indicates the row will be  in bold, spanned through all columns and a grey background
#' in the word \cr
#' \tab \cr
#' \code{1} \tab indicates the row will be in bold \cr
#' \tab \cr
#' \code{2} \tab the row will be in bold and spanned through all columns \cr
#' \tab \cr
#' \code{3} \tab indicates the row of the first column will be indented \cr
#' }
#'
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' \code{\link{stat_tab}}
#' \code{\link{sumby}}
#' \code{\link{dump_missing_report}}
#' \code{\link{get_missing_report}}
#' \code{\link{render_numeric}}
#' \code{\link{render_cat}}
#' @return A matrix with `cttab` class.
#'
#' @example inst/examples/cttab.R
#'
#' @export
#'

cttab <- function(x, ...) {
  UseMethod("cttab")
}

#' @describeIn cttab The default interface, where \code{x} is a \code{data.frame}.
#' @export
cttab.default <- function(x,
                          data,
                          group = NULL,
                          row_split = NULL,
                          total = TRUE,
                          select = NULL,
                          add_missing = TRUE,
                          add_obs = TRUE,
                          digits = getOption("cctu_digits", default = 3),
                          digits_pct = getOption("cctu_digits_pct", default = 0),
                          rounding_fn = signif_pad,
                          subjid_string = getOption("cctu_subjid_string", default = "subjid"),
                          print_plot = getOption("cctu_print_plot", default = TRUE),
                          render_num = getOption("cctu_render_num", default = "Median [Min, Max]"),
                          logical_na_impute = c(FALSE, NA, TRUE),
                          blinded = getOption("cctu_blinded", default = FALSE),
                          ...) {
  .cttab.internal(
    vars = x,
    data = data,
    group = group,
    row_split = row_split,
    total = total,
    select = select,
    add_missing = add_missing,
    add_obs = add_obs,
    digits = digits,
    digits_pct = digits_pct,
    rounding_fn = rounding_fn,
    subjid_string = subjid_string,
    print_plot = print_plot,
    render_num = render_num,
    logical_na_impute = logical_na_impute,
    blinded = blinded
  )
}

#' @describeIn cttab The formula interface, where \code{x} is a \code{formula}.
#' @export
cttab.formula <- function(x,
                          data,
                          total = TRUE,
                          select = NULL,
                          add_missing = TRUE,
                          add_obs = TRUE,
                          digits = getOption("cctu_digits", default = 3),
                          digits_pct = getOption("cctu_digits_pct", default = 0),
                          rounding_fn = signif_pad,
                          subjid_string = getOption("cctu_subjid_string", default = "subjid"),
                          print_plot = getOption("cctu_print_plot", default = TRUE),
                          render_num = getOption("cctu_render_num", default = "Median [Min, Max]"),
                          logical_na_impute = c(FALSE, NA, TRUE),
                          blinded = getOption("cctu_blinded", default = FALSE),
                          ...) {
  f <- split_formula(x)
  logical_na_impute <- logical_na_impute[1]

  if (is.null(f$lhs)) {
    stop("No variables provided to summarise, please add variable to the left hand side of the formula.")
  }

  if (length(f$lhs) != 1) {
    stop("Invalid formula, only `+` is allowed to list multiple variables.")
  }

  if (!length(f$rhs) %in% c(1, 2)) {
    stop("Invalid formula, multiple split provided.")
  }

  if (f$rhs[[1]] == ".") {
    stop("Invalid formula, dot is not allowed.")
  }

  group <- if (f$rhs[[1]] == 1) NULL else all.vars(f$rhs[[1]])
  vars <- all.vars(f$lhs[[1]])

  if (length(f$rhs) == 2) {
    row_split <- all.vars(f$rhs[[2]])
  } else {
    row_split <- NULL
  }

  .cttab.internal(
    vars = vars,
    data = data,
    group = group,
    row_split = row_split,
    total = total,
    select = select,
    add_missing = add_missing,
    add_obs = add_obs,
    digits = digits,
    digits_pct = digits_pct,
    rounding_fn = rounding_fn,
    subjid_string = subjid_string,
    print_plot = print_plot,
    render_num = render_num,
    logical_na_impute = logical_na_impute,
    blinded = blinded
  )
}

.cttab.internal <- function(
    vars,
    data,
    group = NULL,
    row_split = NULL,
    total = TRUE,
    select = NULL,
    add_missing = TRUE,
    add_obs = TRUE,
    digits = getOption("cctu_digits", default = 3),
    digits_pct = getOption("cctu_digits_pct", default = 0),
    rounding_fn = signif_pad,
    subjid_string = getOption("cctu_subjid_string", default = "subjid"),
    print_plot = getOption("cctu_print_plot", default = TRUE),
    render_num = getOption("cctu_render_num", default = "Median [Min, Max]"),
    logical_na_impute = c(FALSE, NA, TRUE),
    blinded = getOption("cctu_blinded", default = FALSE)) {
  # tpcall <- match.call()
  logical_na_impute <- logical_na_impute[1]
  stopifnot(logical_na_impute %in% c(FALSE, NA, TRUE))

  if (blinded) {
    group <- NULL
  }

  vars_list <- c(unlist(vars), group, row_split)
  if (!all(vars_list %in% names(data))) {
    stop(
      "Variable ",
      paste(vars_list[!vars_list %in% names(data)], collapse = ", "),
      " not in the dataset, please check!"
    )
  }

  # Convert to data.table to avoid format lose.
  data <- data.table::as.data.table(data)

  # group variable to factor
  if (!is.null(group)) {
    # Remove missing records for group
    # data <- data[!is.na(data[[group]]), ]
    data <- data[!is.na(data[[group]]), env = list(group = I(group))]

    if (has.labels(data[[group]]) || !is.factor(data[[group]])) {
      data[[group]] <- to_factor(data[[group]], drop.levels = TRUE)
    }
  }

  if (base::anyDuplicated(vars_list)) {
    stop("The variable list, group or row split variable have duplicated variable.")
  }

  if (!is.null(row_split)) {
    if (has.labels(data[[row_split]]) || !is.factor(data[[row_split]])) {
      data[[row_split]] <- to_factor(data[[row_split]], drop.levels = TRUE)
    }
  }

  # Blank cttab matrix
  blnk_cttab <- function(row_labs, pos, from_tab) {
    to_insert <- matrix(c(rep("", ncol(from_tab))),
      nrow = 1,
      dimnames = list(row_labs, colnames(from_tab))
    )

    structure(to_insert,
      position = pos,
      class = class(from_tab)
    )
  }

  # Wrapped tabulation function
  calc_tab <- function(data) {
    # If variables are not list
    if (is.list(vars)) {
      res <- lapply(seq_along(vars), function(i) {
        x <- vars[[i]]

        r <- stat_tab(
          vars = x,
          group = group,
          data = data,
          total = total,
          select = select,
          add_missing = add_missing,
          digits = digits,
          digits_pct = digits_pct,
          rounding_fn = rounding_fn,
          render_num = render_num,
          logical_na_impute = logical_na_impute
        )

        # Add grouping_varing
        if (!is_empty(names(vars)[i])) {
          to_insert <- blnk_cttab(
            row_labs = names(vars)[i],
            pos = 0,
            from_tab = r
          )
          r <- rbind(to_insert, r)
        }

        return(r)
      })

      res <- do.call(rbind, res)

      # This is for logical value that has no variable name, use the grouping_varing
      # label as the variable name
      pos <- attr(res, "position")
      ps <- which(pos == 0 & c(pos[-1], 3) == 1)
      if (any(!is_empty(ps))) {
        pos[ps] <- rep(2, length(ps))
        attr(res, "position") <- pos
      }
    } else {
      res <- stat_tab(
        vars = vars,
        group = group,
        data = data,
        total = total,
        select = select,
        add_missing = add_missing,
        digits = digits,
        digits_pct = digits_pct,
        rounding_fn = rounding_fn,
        render_num = render_num,
        logical_na_impute = logical_na_impute
      )
    }

    # Add observation row
    if (!is.null(group)) {
      gp_tab <- table(data[[group]])
      if (total) {
        gp_tab <- c(gp_tab, "Total" = length(data[[group]]))
      }

      if (add_obs) {
        obs <- matrix(gp_tab,
          nrow = 1,
          dimnames = list("Observation", names(gp_tab))
        )
        obs <- structure(obs,
          position = 1,
          class = c("cttab", class(obs))
        )
        res <- rbind(obs, res)
      }
    }

    return(res)
  }

  # Get arguments that will be passed to plot printing
  if (print_plot) {
    cctab_plot(vars, data, group, row_split, select)
  }

  # If no split
  if (is.null(row_split)) {
    tbody <- calc_tab(data)

    # Report missing
    if (add_missing && !is.null(subjid_string)) {
      miss_rep <- report_missing(
        data = data, vars = vars, select = select,
        subjid_string = subjid_string
      )


      cctu_env$missing_report_data <- rbind(
        cctu_env$missing_report_data,
        miss_rep
      )
    }
  } else {
    # Extract split variable label
    split_lab <- ifelse(has.label(data[[row_split]]),
      var_lab(data[[row_split]]),
      row_split
    )

    dfm <- split(data, data[[row_split]])

    tbody <- lapply(names(dfm), function(x) {
      out <- calc_tab(dfm[[x]])

      to_insert <- blnk_cttab(
        row_labs = paste(split_lab, "=", x),
        pos = 0,
        from_tab = out
      )

      out <- rbind(to_insert, out)

      # Report missing
      if (add_missing && !is.null(subjid_string)) {
        miss_rep <- report_missing(
          data = dfm[[x]], vars = vars, select = select,
          subjid_string = subjid_string
        )
        if (nrow(miss_rep) != 0) {
          miss_rep$visit_var <- row_split
          miss_rep$visit_label <- split_lab
          miss_rep$visit <- x
          cctu_env$missing_report_data <- rbind(
            cctu_env$missing_report_data,
            miss_rep
          )
        }
      }

      return(out)
    })

    tbody <- do.call("rbind", tbody)
  }

  return(tbody)
}


#' Generate a descriptive summary statistics table.
#'
#'
#' It is important to use variable label and value label to produce a proper
#' descriptive table. Variables with value labels will be converted to ordered
#' factor with same order as the value labels (\code{to_factor}). And variable
#' labels will be used in the output. The first row will be blank with row names
#' of variable label. Variable name will be used if the variable does not have
#' a variable label.
#'
#'
#' @inheritParams cttab
#'
#' @return An object of class "cttab".
#'
#' @importFrom data.table .SD
#' @keywords internal

stat_tab <- function(vars,
                     group = NULL,
                     data,
                     total = TRUE,
                     select = NULL,
                     add_missing = TRUE,
                     digits = 2,
                     digits_pct = 1,
                     rounding_fn = signif_pad,
                     render_num = "Median [Min, Max]",
                     logical_na_impute = FALSE) {
  # mf <- match.call()
  data <- as.data.table(data)

  vars_list <- c(unlist(vars), group)
  if (!all(vars_list %in% names(data))) {
    stop(
      "Variable ",
      paste(vars_list[!vars_list %in% names(data)], collapse = ", "),
      " not in the dataset, please check!"
    )
  }


  # group variable to factor
  if (!is.null(group)) {
    # Select records with non-missing group and row split
    # data <- data[!is.na(data[[group]]), , drop = FALSE]
    data <- data[!is.na(data[[group]]), env = list(group = I(group))]

    if (has.labels(data[[group]]) || !is.factor(data[[group]])) {
      data[[group]] <- to_factor(data[[group]], drop.levels = TRUE)
    }
  }

  # Create value labels for characters variables to avoid missing levels between grouping_vars
  convcols <- names(Filter(is.character, data))

  # Get variable class, make sure the class is consistent across data
  var_class <- sapply(vars, function(v) {
    if (!inherits(data[[v]], c("numeric", "integer", "factor", "character", "logical"))) {
      stop(paste("The class of variable", v, "is", class(data[[v]]), "and not supported!"))
    }

    fcase(
      inherits(data[[v]], c("factor", "character")) | has.labels(data[[v]]), "category",
      inherits(data[[v]], c("numeric", "integer")) | all(is.na(data[[v]])), "numeric",
      inherits(data[[v]], c("logical")), "logical"
    )
  })

  if (length(convcols) > 0) {
    data[, convcols] <- data[, lapply(.SD, to_character), .SDcols = convcols]
  }

  # Check if missing
  any_miss <- sapply(vars, function(v) sum(is.na(data[[v]]))) > 0

  # Transform data to list for loop
  if (total && !is.null(group)) {
    x <- c(split(data, data[[group]]), list(Total = data))
  } else if (!is.null(group)) {
    x <- split(data, data[[group]])
  } else {
    x <- list(Total = data)
  }


  r <- do.call(rbind, lapply(vars, function(v) {
    # Get variable label
    variable <- ifelse(has.label(data[[v]]), var_lab(data[[v]]), v)

    y <- do.call(cbind, lapply(x, function(s) {
      z <- s[gen_selec(s, v, select[v]), ] # Apply subset
      z <- z[[v]]

      # Convert character to factor
      if (has.labels(z) | is.character(z)) {
        z <- to_factor(z, ordered = TRUE)
      }


      if (var_class[v] == "category") {
        r <- c("", unlist(render_cat(z, digits_pct = digits_pct)))
      }

      if (var_class[v] == "logical") {
        # Impute missing data for logical
        z[is.na(z)] <- logical_na_impute

        r <- with(
          cat_stat(z, digits_pct = digits_pct)$Yes,
          c(Missing = sprintf("%s/%s (%s)", FREQ, N, PCTnoNA))
        )
        add_missing <- FALSE
      }

      if (var_class[v] == "numeric") {
        r <- c("", unlist(
          render_numeric(z,
                         what = render_num, digits = digits,
                         digits_pct = digits_pct, rounding_fn = rounding_fn
          )
        ))
      }

      names(r)[1] <- variable

      if (add_missing & any_miss[v]) {
        miss <- with(
          cat_stat(is.na(z), digits_pct = digits_pct)$Yes,
          c(Missing = ifelse(FREQ == 0, "",
            sprintf("%s (%s)", FREQ, PCT)
          ))
        )
        r <- c(r, miss)
      }

      return(r)
    }))

    y[y == "NA"] <- ""

    if (nrow(y) == 1 & all(y == "")) {
      return(NULL)
    }

    # Remove Invalid rows
    if (nrow(y) > 1) {
      all_val <- rowSums(apply(y, 2, function(x) x == "")) != ncol(y)
      all_val[1] <- TRUE
      y <- y[all_val, , drop = FALSE]
    }

    # Don't report if the variable has no values to report
    if (nrow(y) == 1 & all(y == "")) {
      return(NULL)
    }

    fst <- ifelse(nrow(y) == 1, 3, 2)

    if (var_class[v] == "logical") {
      fst <- 1
    }

    structure(y,
      position = c(fst, rep(3, nrow(y) - 1)),
      class = c("cttab", class(y))
    )
  }))

  return(r)
}


# Generate selection vector function
# Evaluate the select in the data and generate a logical vector.
gen_selec <- function(data, var, select = NULL) {
  if (is.null(select) || !var %in% names(select)) {
    return(rep(TRUE, length(data[[var]])))
  } else {
    r <- eval(str2expression(select[var]), envir = data)
    r & !is.na(r)
  }
}
