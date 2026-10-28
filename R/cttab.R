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
#' @param nest Controls the row hierarchy when \code{row_split} is supplied.
#'   Default \code{"split"} renders the row-split variable as the outer
#'   header with the analysis variables nested inside it. Use \code{"var"}
#'   to flip the hierarchy so each analysis variable becomes the outer
#'   section and the row-split levels are nested as sub-sections under it.
#'   Ignored when \code{row_split} is \code{NULL}.
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
#' effect on the \code{cttab} function. It is an ideal way to set a global settings
#' if you want this to be effective globally. Currently, you can set \code{digits},
#' \code{digits_pct}, \code{subjid_string}, \code{print_plot}, \code{render_num} and
#' \code{blinded}  in \code{\link{cctu_options}}.
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
#' A long-format \code{data.table} with class \code{cttab} is returned. It carries
#' \code{group} and \code{row_split} attributes that record the names of the grouping
#' and splitting variables. The function \code{\link{cttab_format}} converts this
#' table into a \code{data.frame} with a \code{row_style} attribute used by the
#' \code{print} method and \code{\link{write_table}} to render the final report.
#'
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{round_pad}}
#' \code{\link{stat_tab}}
#' \code{\link{cttab_format}}
#' \code{\link{sumby}}
#' \code{\link{dump_missing_report}}
#' \code{\link{get_missing_report}}
#' \code{\link{render_numeric}}
#' \code{\link{render_cat}}
#' \code{\link{cctu_options}}
#' @return A \code{data.table} with class \code{cttab}.
#'
#' @example inst/examples/cttab.R
#'
#' @export
#'

cttab <- function(x, ...) {
  UseMethod("cttab")
}


#' @describeIn cttab The default interface, where \code{x} is a \code{character}
#' vector or a (optionally named) list of character vectors. A named list inserts
#' a banner header row above the corresponding variables.
#' @export
cttab.default <- function(x,
                          data,
                          group = NULL,
                          row_split = NULL,
                          nest = cctu_opt("nest"),
                          total = TRUE,
                          select = NULL,
                          add_missing = TRUE,
                          add_obs = TRUE,
                          digits = cctu_opt("digits"),
                          digits_pct = cctu_opt("digits_pct"),
                          rounding_fn = signif_pad,
                          subjid_string = cctu_opt("subjid_string"),
                          print_plot = cctu_opt("print_plot"),
                          render_num = cctu_opt("render_num"),
                          logical_na_impute = c(FALSE, NA, TRUE),
                          blinded = cctu_opt("blinded"),
                          ...) {

  vars <- x
  nest <- match.arg(nest, choices = c("split", "var"))
  # `nest` only affects layout when `row_split` is supplied; collapse to
  # the default otherwise so downstream code has one shape to handle.
  if (is.null(row_split)) nest <- "split"
  logical_na_impute <- logical_na_impute[1]
  stopifnot(logical_na_impute %in% c(FALSE, NA, TRUE))

  if (blinded) group <- NULL

  vars_list <- c(unlist(vars), group, row_split)
  if (!all(vars_list %in% names(data))) {
    stop("Variable ",
         paste(vars_list[!vars_list %in% names(data)], collapse = ", "),
         " not in the dataset, please check!")
  }

  if (!is.data.table(data)) data <- as.data.table(data)

  if (base::anyDuplicated(vars_list)) {
    stop("The variable list, group or row split variable have duplicated variable.")
  }

  # A grouping variable is only ever valid as `group` / `row_split`, never as
  # an input elsewhere -- the check above already enforces that for `x`. It has
  # to hold for `select` too: filtering a variable by the group is meaningless
  # (arm A's column filtered to arm A, arm B's to nothing), and it used to be
  # silently WRONG rather than refused. `.SD` excludes the `by` columns, so the
  # per-group cells could not see `group` and went unfiltered while the Total
  # chunk -- which is not grouped -- did filter, making Total equal to a single
  # arm instead of the whole population.
  cttab_check_select_vars(select, c(group, row_split))

  # Drop rows with NA in group / row_split before fanning out to stat_tab,
  # cttab_plot, and report_missing so all three see the same population.
  data <- cttab_drop_na_grouping(data, c(group, row_split))

  flat_vars <- unlist(vars, use.names = FALSE)

  tbody <- stat_tab(
    vars = vars,
    group = group,
    row_split = row_split,
    data = data,
    total = total,
    add_obs = add_obs,
    select = select,
    add_missing = add_missing,
    digits = digits,
    digits_pct = digits_pct,
    rounding_fn = rounding_fn,
    render_num = render_num,
    logical_na_impute = logical_na_impute
  )

  if (print_plot) {
    cttab_plot(flat_vars, data, group, row_split, select)
  }

  if (add_missing && !is.null(subjid_string)) {
    report_missing(
      data = data,
      vars = flat_vars,
      select = select,
      row_split = row_split,
      subjid_string = subjid_string
    )
  }

  if (is.null(tbody)) return(invisible(NULL))

  setattr(tbody, "nest", nest)
  class(tbody) <- unique(c("cttab", class(tbody)))

  return(tbody[])
}

#' @describeIn cttab The formula interface, where \code{x} is a \code{formula}.
#' Parses the formula then dispatches to \code{cttab.default}.
#' @export
cttab.formula <- function(x, data, ...) {
  f <- split_formula(x)

  if (is.null(f$lhs)) {
    stop("No variables provided to summarise, please add variable to the left hand side of the formula.")
  }
  if (length(f$lhs) != 1) {
    stop("Invalid formula, only `+` is allowed to list multiple variables.")
  }
  if (!length(f$rhs) %in% c(1, 2)) {
    stop("Invalid formula, multiple split provided.")
  }
  if (identical(f$rhs[[1]], as.name("."))) {
    stop("Invalid formula, dot is not allowed.")
  }

  group <- if (identical(f$rhs[[1]], 1) ||
               identical(f$rhs[[1]], 1L)) {
    NULL
  } else {
    all.vars(f$rhs[[1]])
  }
  vars  <- all.vars(f$lhs[[1]])
  row_split <- if (length(f$rhs) == 2) all.vars(f$rhs[[2]]) else NULL

  cttab.default(x = vars, data = data,
                group = group, row_split = row_split, ...)
}


#' Generate a descriptive summary statistics table.
#'
#' Internal long-format summariser that powers \code{\link{cttab}}. Variable
#' value labels are honoured (variables with labels are converted to ordered
#' factors). Variable labels are used in the output when present; the variable
#' name is used otherwise.
#'
#' The returned \code{data.table} has the following columns:
#' \itemize{
#'   \item the row-split variable, when \code{row_split} is supplied;
#'   \item the grouping variable when \code{group} is supplied (a factor whose
#'     levels finish with \code{"Total"} when \code{total = TRUE});
#'   \item \code{Group_ID}, \code{Group_Label}: identifier and (optional) banner
#'     label produced when \code{vars} is a named list;
#'   \item \code{Var_ID}, \code{Variable}: per-variable id (0 = "Observation"
#'     row) and the rendered variable label;
#'   \item \code{Stat_ID}, \code{Statistic}: per-statistic id and label;
#'   \item \code{Value}: the rendered cell value (character)
#' }
#'
#' @inheritParams cttab
#' @param group Name of the grouping variable, length 0/1. When supplied each
#' level becomes a column in the rendered table (plus an extra "Total" column
#' if \code{total = TRUE}).
#' @param row_split Variable used for splitting table rows, length 0/1.
#'
#' @return A long-format \code{data.table}. \code{NULL} when there is nothing
#' to summarise.
#'
#' @import data.table
#' @keywords internal

stat_tab <- function(vars,
                     group = NULL,
                     row_split = NULL,
                     data,
                     total = TRUE,
                     add_obs = FALSE,
                     select = NULL,
                     add_missing = TRUE,
                     digits = 2,
                     digits_pct = 1,
                     rounding_fn = signif_pad,
                     render_num = "Median [Min, Max]",
                     logical_na_impute = FALSE) {

  # data.table NSE helper column, bound locally so the object_usage linter /
  # R CMD check don't treat it as an undefined global.
  .empty <- NULL

  data <- copy(as.data.table(data))

  # Validate variable presence
  flat_vars <- unlist(vars, use.names = FALSE)
  required <- c(flat_vars, group, row_split)
  if (!all(required %in% names(data))) {
    stop(
      "Variable ",
      paste(setdiff(required, names(data)), collapse = ", "),
      " not in the dataset, please check!"
    )
  }

  # Defensive checks for direct callers; cttab.default already performs both
  # (the NA drop with a message) before invoking stat_tab. The select/grouping
  # check is repeated here rather than only upstream because this is where the
  # damage happens -- `.SD` excludes the `by` columns, so a filter naming the
  # group silently went unapplied per group while the ungrouped Total chunk
  # did apply it.
  cttab_check_select_vars(select, c(group, row_split))

  for (g in c(group, row_split)) {
    data <- data[!is.na(data[[g]]), env = list(g = I(g))]
  }
  cttab_factorise(data, c(group, row_split), drop_levels = TRUE)

  # Categorical analysis vars (value-labelled or character) are rendered as
  # ordered factors. We do NOT convert them in place to allow filtering.
  cat_vars <- intersect(
    names(which(vapply(data, function(z) has_labels(z) || is.character(z),
                       logical(1L)))),
    flat_vars
  )
  fac_alias <- setNames(sprintf(".cttab_fac_%s", cat_vars), cat_vars)
  for (v in cat_vars) {
    set(data, j = fac_alias[[v]], value = to_factor(data[[v]], ordered = TRUE))
  }

  # Normalize variable groupings (Group_ID + Group_Label) when vars is a list
  if (is.list(vars)) {
    nm <- names(vars)
    if (is.null(nm)) nm <- rep("", length(vars))
    nm[is.na(nm) | nm == ""] <- NA_character_
    var_group_id <- rep(seq_along(vars), lengths(vars))
    var_group_label <- rep(nm, lengths(vars))
  } else {
    var_group_id <- rep(1L, length(flat_vars))
    var_group_label <- rep(NA_character_, length(flat_vars))
  }

  # Per-chunk processor: returns long-format rows for one (group, row_split)
  # cell of data. Includes the Observation row when add_obs && include_obs.
  process_chunk <- function(.SD, include_obs) {
    rows_list <- list()

    if (include_obs) {
      rows_list[[length(rows_list) + 1L]] <- data.table(
        Group_ID    = 0L,
        Group_Label = NA_character_,
        Var_ID      = 0L,
        Variable    = "Observation",
        Statistic   = "",
        Value       = as.character(nrow(.SD)),
        Stat_ID     = 1L
      )
    }

    for (i in seq_along(flat_vars)) {
      col <- flat_vars[i]

      # Categorical vars render from their aliased factor copy; numeric/logical
      # vars render from the original column. The `select` filter, however,
      # always evaluates against the original columns in `.SD`.
      render_col <- if (col %in% cat_vars) fac_alias[[col]] else col
      render_vec <- data[[render_col]]

      if (!inherits(render_vec, c("numeric", "integer", "factor",
                                  "character", "logical"))) {
        stop(paste("The class of variable", col, "is",
                   class(data[[col]]), "and not supported!"))
      }

      # Apply per-variable filter from `select` (mask from the original column,
      # values from the render column).
      val <- .SD[[render_col]][cttab_eval_select(.SD, col, select)]

      lbl <- cttab_get_label(data[[col]], col)

      # Missing summary (computed for everything but logicals)
      miss <- NULL
      if (add_missing && sum(is.na(val)) > 0) {
        miss <- with(
          cat_stat(is.na(val), digits_pct = digits_pct)$Yes,
          c(Missing = ifelse(FREQ == 0, "", sprintf("%s (%s)", FREQ, PCT)))
        )
      }

      stats <- NULL

      if (inherits(render_vec, c("numeric", "integer"))) {
        stats <- render_numeric(
          val,
          what        = render_num,
          digits      = digits,
          digits_pct  = digits_pct,
          rounding_fn = rounding_fn
        )
      } else if (inherits(render_vec, c("factor", "character"))) {
        stats <- render_cat(val, digits_pct = digits_pct)
      } else if (inherits(render_vec, "logical")) {
        # All-NA logical loses its categorical meaning; fall through to the
        # numeric render path so the user still sees Valid Obs. + Missing.
        if (all(is.na(render_vec))) {
          stats <- render_numeric(
            val,
            what        = render_num,
            digits      = digits,
            digits_pct  = digits_pct,
            rounding_fn = rounding_fn
          )
        } else {
          val[is.na(val)] <- logical_na_impute
          cs <- cat_stat(val, digits_pct = digits_pct)$Yes
          # Single-row logical: keep an unnamed (empty-named) Statistic so the
          # formatter knows to use the variable label as the row name.
          stats <- setNames(
            sprintf("%s/%s (%s)", cs$FREQ, cs$N, cs$PCTnoNA),
            ""
          )
          miss <- NULL
        }
      }

      # Append `miss` BEFORE deciding whether there is anything to report. A
      # categorical with no renderable categories still has to report its
      # missingness: `to_factor()` builds levels from the observed values, so
      # an all-NA / all-blank character yields a 0-level factor and render_cat
      # returns nothing. Testing `stats` first dropped such a variable from the
      # table entirely, leaving the reader unable to tell "not requested" from
      # "100% missing" -- and a labelled-numeric in the same state reported
      # Missing correctly, so the two disagreed on identical data.
      if (length(stats) == 0L) stats <- NULL  # keep c() from returning a list
      stats <- c(stats, miss)

      if (length(stats) == 0L) next

      rows_list[[length(rows_list) + 1L]] <- data.table(
        Group_ID    = var_group_id[i],
        Group_Label = var_group_label[i],
        Var_ID      = i,
        Variable    = lbl,
        Statistic   = names(stats),
        Value       = as.character(stats),
        Stat_ID     = seq_along(stats)
      )
    }

    if (length(rows_list) == 0L) return(NULL)
    rbindlist(rows_list, use.names = TRUE)
  }

  include_obs <- isTRUE(add_obs) && !is.null(group)
  by_cols <- c(group, row_split)  # NULLs drop out

  # The synthetic Total column is written into the group column as the literal
  # string "Total", so a real group level of that name is indistinguishable
  # from it. Left alone this surfaced as a bare "factor level [3] is
  # duplicated" from the level rebuild below; had that not errored first, the
  # cast key would have collided and rendered row counts instead of statistics.
  if (total && !is.null(group) &&
        "Total" %in% as.character(unique(data[[group]]))) {
    stop("The grouping variable '", group, "' has a level named 'Total', ",
         "which collides with the generated Total column. Rename the level, ",
         "or pass total = FALSE.")
  }

  # Single grouped tabulation; the by argument copes with NULL group/row_split.
  long_res <- if (length(by_cols)) {
    data[, process_chunk(.SD, include_obs = include_obs), by = by_cols]
  } else {
    process_chunk(data, include_obs = include_obs)
  }

  # When grouped, append the "Total" column by re-tabulating across groups.
  if (total && !is.null(group)) {
    total_res <- if (!is.null(row_split)) {
      data[, process_chunk(.SD, include_obs = include_obs), by = row_split]
    } else {
      process_chunk(data, include_obs = include_obs)
    }
    if (!is.null(total_res) && nrow(total_res) > 0) {
      total_res[, (group) := "Total"]
      long_res <- rbind(long_res, total_res, fill = TRUE)
    }
  }

  if (is.null(long_res) || nrow(long_res) == 0L) return(NULL)

  # Drop stat rows whose Value is empty across every group cell (within the
  # same row_split, when present). Done first so the := assignments don't
  # collide with shallow copies caused by the factor reassignments below.
  setalloccol(long_res)
  long_res[, .empty := !nzchar(Value)]
  empty_by <- c(if (!is.null(row_split)) row_split, "Var_ID", "Stat_ID")
  long_res[Var_ID > 0L, .empty := all(.empty), by = empty_by]
  long_res <- long_res[!(.empty)]
  setalloccol(long_res)            # subset returns a fresh DT
  long_res[, .empty := NULL]

  if (nrow(long_res) == 0L) return(NULL)

  if (!is.null(group)) {
    grp_orig <- if (is.factor(data[[group]])) levels(data[[group]]) else
      sort(unique(as.character(data[[group]])))
    grp_levels <- c(grp_orig, if (total) "Total")
    set(long_res, j = group,
        value = factor(as.character(long_res[[group]]), levels = grp_levels))
  }

  # Order row_split factor following original data order
  if (!is.null(row_split)) {
    rs_levels <- if (is.factor(data[[row_split]])) levels(data[[row_split]]) else
      sort(unique(as.character(data[[row_split]])))
    set(long_res, j = row_split,
        value = factor(as.character(long_res[[row_split]]), levels = rs_levels))
  }

  # Order: row_split / group / variable groupings first, then "Missing"
  # rows last within each variable, then by Stat_ID.
  ord_cols <- c(if (!is.null(row_split)) row_split,
                if (!is.null(group)) group,
                "Group_ID", "Var_ID", "Stat_ID")
  setorderv(long_res, ord_cols)

  setattr(long_res, "group", group)
  setattr(long_res, "row_split", row_split)
  if (!is.null(row_split)) {
    setattr(long_res, "row_split_label",
            if (has_label(data[[row_split]])) var_lab(data[[row_split]])
            else row_split)
  }

  lead_cols <- intersect(c("Group_ID", "Group_Label", "Var_ID", "Stat_ID"),
                         names(long_res))
  setcolorder(long_res, c(lead_cols, setdiff(names(long_res), lead_cols)))

  long_res[]
}
