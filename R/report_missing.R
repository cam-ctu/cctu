#' Generate missing report
#'
#' This function use the same method as described in the \code{cctab},
#' reporting missingness of the variables. It includes which form is the
#' variable from,  set as `Derived` if not from DLU file. And missing percentage
#'  with which subjects are have missing value for that particular variable.
#'  This is internal function, not intend to use it directly by user.
#'
#' @inheritParams cttab
#' @details This requires access to a data.frame of DLU file, this will be
#' derived from the package environment
#' and should be set using \code{\link{get_dlu}}.
#'
#' @seealso \code{\link{cttab}} \code{\link{dump_missing_report}}
#' \code{\link{get_missing_report}} \code{\link{reset_missing_report}}
#'
#' @return A data frame
#'
#' @keywords internal
report_missing <- function(data,
                           vars,
                           select = NULL,
                           row_split = NULL,
                           subjid_string = getOption("cctu_subjid_string",
                             default = "subjid"
                           )) {
  
  # Blank return
  blnk_miss <- setNames(
    data.frame(matrix(ncol = 8, nrow = 0)),
    c(
      "form", "visit_var", "visit_label", "visit",
      "variable", "label", "missing_pct", "subject_id"
    )
  )

  dlu <- get_dlu()
  # If the subjid can not be found in the dataset
  if (!subjid_string %in% names(data)) {
    return(invisible(NULL))
  }
  
  # Ensure data is data.table
  dt <- as.data.table(data)
  
  vars <- unlist(vars, use.names = FALSE)
  
  # Filter variables that are actually in the data
  vars <- vars[vars %in% names(dt)]
  if(length(vars) == 0) return(invisible(NULL))

  # Check if row_split is valid
  if (!is.null(row_split)) {
    if (!row_split %in% names(dt)) {
      warning("row_split variable not found in data. Ignoring row_split.")
      row_split <- NULL
    }
  }

  res_list <- lapply(vars, function(v) {
      
      # Apply selection
      if (!is.null(select) && v %in% names(select)) {
        filter_expr <- str2expression(select[v])
        rows_to_keep <- tryCatch({
           r <- eval(filter_expr, envir = dt)
           r & !is.na(r)
        }, error = function(e) rep(TRUE, nrow(dt)))
        
        # Subset data based on filter
        d_sub <- dt[rows_to_keep]
      } else {
        d_sub <- dt
      }
      
      # Define grouping
      by_clause <- if(!is.null(row_split)) row_split else NULL
      
      # Aggregation
      agg <- d_sub[, .(
         n_miss = sum(is.na(get(v))),
         n_total = .N,
         subids = list(get(subjid_string)[is.na(get(v))])
      ), by = by_clause]
      
      # Filter out groups with 0 missing
      agg <- agg[n_miss > 0]
      
      if (nrow(agg) == 0) return(NULL)
      
      # Format output
      # Get label
      lbl <- if(has_label(dt[[v]])) var_lab(dt[[v]]) else v
      
      # Get form
      fm_name <- if (!is.null(dlu) && v %in% dlu$shortcode) {
        dlu$form[dlu$shortcode == v]
      } else {
        "Derived"
      }
      
      # Get split label
      if (!is.null(row_split)) {
         split_lbl <- if(has_label(dt[[row_split]])) var_lab(dt[[row_split]]) else row_split
         visit_val <- as.character(agg[[row_split]])
         visit_var <- row_split
      } else {
         split_lbl <- NA
         visit_val <- NA
         visit_var <- NA
      }

      # Construct result
      out <- data.frame(
        form = fm_name,
        visit_var = visit_var,
        visit_label = split_lbl,
        visit = visit_val,
        variable = v,
        label = lbl,
        missing_pct = paste0(round(100 * agg$n_miss / agg$n_total, 1), "% (", agg$n_miss, "/", agg$n_total, ")"),
        subject_id = sapply(agg$subids, paste, collapse = ", "),
        stringsAsFactors = FALSE
      )
      return(out)
  })
    
  res <- do.call(rbind, res_list)
  if (!is.null(res)) {
    cctu_env$missing_report_data <- rbind(
      cctu_env$missing_report_data,
      res
    )
  }
  invisible(NULL)
}

#' @name dump_missing_report
#' @aliases get_missing_report
#' @aliases reset_missing_report
#' @title Save/Get/Reset missingness report
#'
#' @description
#' \code{dump_missing_report} can be used to save the missingness report to
#' a file.
#' \code{get_missing_report} Return the missingness report data.
#' \code{reset_missing_report} Reset the internal missingness report data to
#'  blank.
#'
#' @param x File path the report will be dumped to. Default is under `Output`
#' folder, named as `variable_missing_report.csv`.
#' @seealso \code{\link{cttab}} \code{\link{report_missing}}
#' @export
#'
dump_missing_report <- function(x = "Output/variable_missing_report.csv") {
  utils::write.csv(get_missing_report(),
    file = x,
    na = "", row.names = FALSE
  )
}

#' @rdname dump_missing_report
#' @export
get_missing_report <- function() {
  unique(cctu_env$missing_report_data)
}

#' @rdname dump_missing_report
#' @export
reset_missing_report <- function() {
  cctu_env$missing_report_data <- setNames(
    data.frame(matrix(ncol = 8, nrow = 0)),
    c(
      "form", "visit_var", "visit_label",
      "visit", "variable", "label", "missing_pct",
      "subject_id"
    )
  )
}
