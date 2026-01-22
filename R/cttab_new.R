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

cttabnew <- function(x, ...) {
  UseMethod("cttabnew")
}


#' @describeIn cttabnew The default interface, where \code{x} is a \code{character}.
#' @export
cttabnew.default <- function(x,
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
  .cttabnew_internal(
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

#' @describeIn cttabnew The formula interface, where \code{x} is a \code{formula}.
#' @export
cttabnew.formula <- function(x,
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

  .cttabnew_internal(
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

.cttabnew_internal <- function(
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
  if(!is.data.table(data))data <- as.data.table(data)

  # group variable to factor
  if (!is.null(group)) {
    # Remove missing records for group
    # data <- data[!is.na(data[[group]]), ]
    data <- data[!is.na(data[[group]]), env = list(group = I(group))]

    if (has_labels(data[[group]]) || !is.factor(data[[group]])) {
      data[[group]] <- to_factor(data[[group]], drop_levels = TRUE)
    }
  }

  if (base::anyDuplicated(vars_list)) {
    stop("The variable list, group or row split variable have duplicated variable.")
  }

  if (!is.null(row_split)) {
    if (has_labels(data[[row_split]]) || !is.factor(data[[row_split]])) {
      data[[row_split]] <- to_factor(data[[row_split]], drop_levels = TRUE)
    }
  }

  # Convert variables to factor with value labels
  cols_toconvert <- sapply(data, function(x) has_labels(x) | is.character(x))
  cols_toconvert <- intersect(names(cols_toconvert)[cols_toconvert], vars)
  data[,(cols_toconvert):= lapply(.SD, to_factor, ordered = TRUE), .SDcols = cols_toconvert]

  # Summarise data
  tbody <- stat_tab_new(
    vars = vars,
    group_vars = c(group, row_split),
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

  # Get arguments that will be passed to plot printing
  if (print_plot) {
    cctab_plot(vars, data, group, row_split, select)
  }

  # Report missing -------------------
  if (add_missing && !is.null(subjid_string)) {
    report_missing(
      data = data,
      vars = vars,
      select = select,
      row_split = row_split,
      subjid_string = subjid_string
    )
  }
  # End of missing report -----------------

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
#' @param group_vars Name of the grouping variable, length one or two character vector. 
#' If two variables are provided, the first one will be used for grouping and the second
#' one will be used for row split. The row split variable should be the last element of the
#' \code{group_vars} vector.
#'
#' @return An object of class "cttab".
#'
#' @import data.table
#' @keywords internal

stat_tab_new <- function(
  vars,
  group_vars = NULL, # Row split variable should be included as the last element in group
  data,
  total = TRUE,
  select = NULL,
  add_missing = TRUE,
  digits = 2,
  digits_pct = 1,
  rounding_fn = signif_pad,
  render_num = "Median [Min, Max]",
  logical_na_impute = FALSE
) {

  process_chunk <- function(.SD) {

    n_row <- data.table(
      Variable  = "Observation",
      Statistic = "",
      Value     = as.character(nrow(.SD)),
      Var_ID    = 0,
      Stat_ID   = 1
    )

    # Iterate via Index (i) to capture the Variable Order
    chunk_list <- lapply(seq_along(vars), function(i) {
      col <- vars[i]

      if(!inherits(data[[col]], c("numeric", "integer", "factor", "character", "logical"))){
        stop(paste("The class of variable", col, "is", class(data[[col]]), "and not supported!"))
      }

      # Check if this column has a specific filter in 'select'
      if (!is.null(select) && col %in% names(select)) {

        filter_string <- select[[col]]

        # We use tryCatch to prevent crashing if the string is invalid
        rows_to_keep <- tryCatch({
          expr <- parse(text = filter_string)
          # Evaluate expression within .SD environment
          # resulting in a TRUE/FALSE vector
          eval(expr, envir = .SD)
        }, error = function(e) {
          warning(paste("Filter failed for variable:", col, "-", e$message))
          return(rep(TRUE, nrow(.SD))) # Fallback: keep all rows if error
        })

        # Handle NA results in filtering (treat as FALSE)
        rows_to_keep[is.na(rows_to_keep)] <- FALSE

        # Subset the values
        val <- .SD[[col]][rows_to_keep]

      } else {
        # Standard case: Use all data in this group
        val <- .SD[[col]]
      }
      # ------------------------------------------

      # Get variable label
      if(has_label(data[[col]]))
        lbl <- var_lab(data[[col]])
      else
        lbl <- col

      # Handle Missingness
      if (add_missing & sum(is.na(val)) > 0) {
        miss <- with(
          cat_stat(is.na(val), digits_pct = digits_pct)$Yes,
          c(Missing = ifelse(FREQ == 0, "",
            sprintf("%s (%s)", FREQ, PCT)
          ))
        )
      }else {
        miss <- NULL
      }

      stats <- NULL
      # Handle Numeric variable statistics
      if (inherits(data[[col]], c("numeric", "integer")) | all(is.na(data[[col]]))) {
        stats <- render_numeric(
          val,
          what = render_num,
          digits = digits,
          digits_pct = digits_pct,
          rounding_fn = rounding_fn
        )
      }

      # Handle Factor/Character variable statistics
      if(inherits(data[[col]], c("factor", "character"))) {
        stats <- render_cat(val, digits_pct = digits_pct)
      }

      # Handle Logical variable statistics
      if(inherits(data[[col]], "logical")) {
        val[is.na(val)] <- logical_na_impute
        stats <- with(
          cat_stat(val, digits_pct = digits_pct)$Yes,
          c(Missing = sprintf("%s/%s (%s)", FREQ, N, PCTnoNA))
        )
        miss <- NULL
      }

      if (is.null(stats)) return(NULL)

      stats <- c(stats, miss)

      # --- Calculate Stat_IDs with "Missing" forced to bottom ---
      s_names <- names(stats)
      s_ids   <- seq_along(stats)

      # If "Missing" exists, override its ID to 9999
      # This ensures it is sorted last, even if other stats are added later
      if ("Missing" %in% s_names) {
        s_ids[s_names == "Missing"] <- 9999
      }

      # Create Table with Ordering IDs
      data.table(
        Variable  = lbl,
        Statistic = names(stats),
        Value     = as.character(stats),
        Var_ID    = i,
        Stat_ID   = s_ids
      )
    })

    # Remove NULLs and bind
    chunk_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, chunk_list)
    if (length(chunk_list) == 0) return(NULL)
    rbindlist(c(list(n_row), chunk_list))
  }

  # --- Aggregate ---
  if (is.null(group_vars)) {
    long_res <- process_chunk(data)
    setnames(long_res, "Value", "Total")
  } else {
    stopifnot(length(group_vars) %in% c(1,2))
    if(total){
      long_res <- data[, process_chunk(.SD), by = group_vars]
      if(length(group_vars) > 1){
        total_res <- data[, process_chunk(.SD), by = eval(group_vars[2])]
      } else {
        total_res <- process_chunk(data)
      }
      total_res[[group_vars[1]]] <- "Total"
      long_res <- rbind(long_res, total_res, fill = TRUE)
      # Handle ordering
      long_res[[group_vars[1]]] <- factor(long_res[[group_vars[1]]],
                                     levels = c(setdiff(unique(long_res[[group_vars[1]]]), "Total"), "Total"))
      }
  }

  # --- Enforce Order for Pivoting ---
  setorder(long_res, Var_ID, Stat_ID)
  long_res[, Variable := factor(Variable, levels = unique(Variable))]
  long_res[, Statistic := factor(Statistic, levels = unique(Statistic))]

  return(long_res)
}


