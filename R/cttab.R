

#' Generate an table of descriptive statistics.
#'
#' This is a wrapper function of \code{stat_tab}, allowing for groupped variables,
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
#'  splited using this variable. Useful for repeated measures.
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
#' @param ... Not used.
#' @details
#' Some of the function parameters can be set with options. This will have an global
#' @details
#' Some of the function parameters can be set with options. This will have an global
#' effect on the \code{cctab} function. It is an ideal way to set a global settings
#' if you want this to be effctive globally. Currently, you can set \code{digits},
#' \code{digits_pct}, \code{subjid_string} and \code{print_plot}  by adding \code{"cctu_"}
#'  prefix in the \code{options}. For example, you can suppress the plot
#' from printting by setting \code{options(cctu_print_plot = FALSE)}.
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
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' \code{\link{stat_tab}}
#' \code{\link{sumby}}
#' \code{\link{dump_missing_report}}
#' \code{\link{get_missing_report}}
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
                          ...) {
    .cttab.internal(vars = x,
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
                    print_plot =print_plot)
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
                          ...) {

    f <- split_formula(x)

    if(is.null(f$lhs))
      stop("No variables provided to summarise, please add variable to the left hand side of the formula.")

    if(length(f$lhs) != 1)
      stop("Invalid formula, only `+` is allowed to list multiple variables.")

    if(!length(f$rhs) %in% c(1, 2))
      stop("Invalid formula, multiple split provided.")

    if(f$rhs[[1]] == ".")
      stop("Invalid formula, dot is not allowed.")

    group <- if(f$rhs[[1]] == 1) NULL else all.vars(f$rhs[[1]])
    vars <- all.vars(f$lhs[[1]])

    if(length(f$rhs) == 2)
      row_split <- all.vars(f$rhs[[2]])
    else
      row_split <- NULL

    .cttab.internal(vars = vars,
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
                    print_plot =print_plot)
}

.cttab.internal <- function(vars,
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
                            print_plot = getOption("cctu_print_plot", default = TRUE)) {

  tpcall <- match.call()

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

  # Group variable to factor
  if (!is.null(group)) {

    # Remove missing records for group
    data <- data[!is.na(data[[group]]), ]

    if (has.labels(data[[group]]) | !is.factor(data[[group]]))
      data[[group]] <- to_factor(data[[group]], drop.levels = TRUE)
  }

  if(base::anyDuplicated(vars_list))
    stop("The variable list, group or row split variable have duplicated variable.")

  if (!is.null(row_split)) {
    if (has.labels(data[[row_split]]) | !is.factor(data[[row_split]]))
      data[[row_split]] <- to_factor(data[[row_split]], drop.levels = TRUE)
  }

  # Blank cttab matrix
  blnk_cttab <- function(row_labs, pos, from_tab){
    to_insert <- matrix(c(rep("", ncol(from_tab))), nrow = 1,
                        dimnames = list(row_labs, colnames(from_tab)))

    structure(to_insert,
              position = pos,
              class = class(from_tab))
  }

  # Wrapped tabulation function
  calc_tab <- function(dat){

    # If variables are not list
    if(is.list(vars)){

      res <- lapply(seq_along(vars), function(i){

        x <- vars[[i]]

        r <- stat_tab(vars = x,
                      group  = group,
                      data   = dat,
                      total  = total,
                      select = select,
                      add_missing = add_missing,
                      digits = digits,
                      digits_pct = digits_pct,
                      rounding_fn = rounding_fn)

        # Add grouping
        if(!is_empty(names(vars)[i])){
          to_insert <- blnk_cttab(row_labs = names(vars)[i],
                                 pos = 0,
                                 from_tab = r)
          r <- rbind(to_insert, r)
        }

        return(r)

      })

      res <- do.call(rbind, res)

      # This is for logical value that has no variable name, use the grouping
      # label as the variable name
      pos <- attr(res, "position")
      ps <- which(pos == 0 & c(pos[-1], 3) == 3)
      if(any(!is_empty(ps))){
        pos[ps] <- rep(2, length(ps))
        attr(res, "position") <- pos
      }

    }else{
      res <- stat_tab(vars = vars,
                      group  = group,
                      data   = dat,
                      total  = total,
                      select = select,
                      add_missing = add_missing,
                      digits = digits,
                      digits_pct = digits_pct,
                      rounding_fn = rounding_fn)
    }

    # Add observation row
    if(!is.null(group)){
      gp_tab <- table(dat[[group]])
      if(total)
        gp_tab <- c(gp_tab, "Total" = length(dat[[group]]))

      if(add_obs){
        obs <- matrix(gp_tab, nrow = 1,
                      dimnames = list("Observation", names(gp_tab)))
        obs <- structure(obs,
                         position = 1,
                         class =  c("cttab", class(obs)))
        res <- rbind(obs, res)
      }

    }

    return(res)
  }

  # Get arguments that will be passed to plot printing
  if(print_plot){
    cctab_plot(vars, data, group, row_split, select)
  }

  # If no split
  if (is.null(row_split)) {
    tbody <- calc_tab(data)

    # Report missing
    if(add_missing && !is.null(subjid_string)){
      miss_rep <- report_missing(data = data, vars = vars, select = select,
                                 subjid_string = subjid_string)


      cctu_env$missing_report_data <- rbind(cctu_env$missing_report_data,
                                          miss_rep)
    }


  } else{

    # Extract split variable label
    split_lab <- ifelse(has.label(data[[row_split]]),
                        var_lab(data[[row_split]]),
                        row_split)

    dfm <- split(data, data[[row_split]])

    tbody <- lapply(names(dfm), function(x) {
      out <- calc_tab(dfm[[x]])

      to_insert <- blnk_cttab(row_labs = paste(split_lab, "=", x),
                             pos = 0,
                             from_tab = out)

      out <- rbind(to_insert, out)

      # Report missing
      if(add_missing && !is.null(subjid_string)){
        miss_rep <- report_missing(data = dfm[[x]], vars = vars, select = select,
                                   subjid_string = subjid_string)
        if(nrow(miss_rep) != 0){
          miss_rep$visit_var <- row_split
          miss_rep$visit_label <- split_lab
          miss_rep$visit <- x
          cctu_env$missing_report_data <- rbind(cctu_env$missing_report_data,
                                                miss_rep)
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
                     total  = TRUE,
                     select = NULL,
                     add_missing = TRUE,
                     digits = 2,
                     digits_pct = 1,
                     rounding_fn = signif_pad){

  mf <- match.call()

  vars_list <- c(unlist(vars), group)
  if (!all(vars_list %in% names(data))) {
    stop(
      "Variable ",
      paste(vars_list[!vars_list %in% names(data)], collapse = ", "),
      " not in the dataset, please check!"
    )
  }


  # Group variable to factor
  if (!is.null(group)) {

    # Select records with non-missing group and row split
    data <- data[!is.na(data[[group]]), , drop = FALSE]

    if (has.labels(data[[group]]) | !is.factor(data[[group]]))
      data[[group]] <- to_factor(data[[group]], drop.levels = TRUE)

  }

  # Create value labels for characters variables to avoid missing levels between groups
  convcols <- names(Filter(is.character, data))

  if(length(convcols) > 0)
    data[, convcols] <- data[,lapply(.SD, to_character), .SDcols = convcols]

  # Check if missing
  any_miss <- sapply(vars, function(v)sum(is.na(data[[v]]))) > 0

  # Transform data to list for loop
  if (total & !is.null(group)) {
    x <- c(split(data, data[[group]]), list(Total = data))
  } else if (!is.null(group)) {
    x <- split(data, data[[group]])
  } else{
    x <- list(Total = data)
  }


  r <- do.call(rbind, lapply(vars, function(v){

    # Get variable label
    variable <- ifelse(has.label(data[[v]]), var_lab(data[[v]]), v)

    y <- do.call(cbind, lapply(x, function(s) {

      z <- s[gen_selec(s, v, select[v]), ] # Apply subset
      z <- z[[v]]

      # Convert character to factor
      if(has.labels(z) | is.character(z))
        z <- to_factor(z, ordered = TRUE)

      if(!inherits(z, c("numeric", "integer", "factor", "character", "logical")))
        stop(paste("The class of variable", v, "is", class(z), "and not supported!"))

      if(inherits(z, c("factor", "character"))){
        r <- c("", render_cat(z, digits_pct = digits_pct))
      }

      if(inherits(z, "logical")){
        r <- miss <- with(cat_stat(z, digits_pct = digits_pct)$Yes,
                          c(Missing = ifelse(FREQ == 0, "",
                                             sprintf("%s/%s (%s)", FREQ, Nall, PCT))))
        add_missing <- FALSE
      }

      if(inherits(z, c("numeric", "integer"))){
        r <- c("", render_numeric(z, digits = digits, digits_pct = digits_pct, rounding_fn = rounding_fn))
      }

      names(r)[1] <- variable

      if(add_missing & any_miss[v]){
        miss <- with(cat_stat(is.na(z), digits_pct = digits_pct)$Yes,
                     c(Missing = ifelse(FREQ == 0, "",
                                        sprintf("%s (%s)", FREQ, PCT))))
        r <- c(r, miss)
      }

      return(r)

    }))

    y[y == "NA"] <- ""

    if(nrow(y) == 1 & all(y == ""))
      return(NULL)

    # Remove Invalid rows
    if(nrow(y) >1){
      all_val <- rowSums(apply(y, 2, function(x)x == "")) != ncol(y)
      all_val[1] <- TRUE
      y <- y[all_val, ,drop = FALSE]
    }

    # Don't report if the variable has no values to report
    if(nrow(y) == 1 & all(y == ""))
      return(NULL)

    fst <- ifelse(nrow(y) == 1, 3, 2)

    structure(y,
              position = c(fst, rep(3, nrow(y) - 1)),
              class = c("cttab", class(y)))
  }))

  return(r)

}


# Generate selection vector function
# Evaluate the select in the data and generate a logical vector.
gen_selec <- function(dat, var, select = NULL) {
  if (is.null(select) | !var %in% names(select)) {
    return(rep(TRUE, length(dat[[var]])))
  } else{
    r <- eval(str2expression(select[var]), envir = dat)
    r & !is.na(r)
  }
}
