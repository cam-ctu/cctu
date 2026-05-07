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
                           subjid_string = cctu_opt("subjid_string")) {

  # If the subjid column is missing there's nothing meaningful to report.
  if (!subjid_string %in% names(data)) return(invisible(NULL))

  vars <- unlist(vars, use.names = FALSE)
  vars <- vars[vars %in% names(data)]
  if (length(vars) == 0L) return(invisible(NULL))

  if (!is.null(row_split) && !row_split %in% names(data)) {
    warning("row_split variable not found in data. Ignoring row_split.")
    row_split <- NULL
  }

  # Local copy so the factor coercion below doesn't leak into the caller's
  # data.table.
  dt <- data.table::copy(data.table::as.data.table(data))

  # Coerce row_split to a factor when labelled-numeric so `as.character()` on
  # the aggregated values returns the visit *labels* (e.g. "Baseline") rather
  # than the underlying numeric codes.
  cttab_factorise(dt, row_split)

  dlu <- get_dlu()

  res_list <- lapply(vars, function(v) {
    keep <- cttab_eval_select(dt, v, select)
    d_sub <- dt[keep]

    agg <- d_sub[, .(
      n_miss  = sum(is.na(.v)),
      n_total = .N,
      subids  = list(.sid[is.na(.v)])
    ), by = row_split, env = list(.v = v, .sid = subjid_string)]
    agg <- agg[n_miss > 0]
    if (nrow(agg) == 0L) return(NULL)

    fm_name <- if (!is.null(dlu) && v %in% dlu$shortcode) {
      dlu$form[dlu$shortcode == v]
    } else {
      "Derived"
    }

    if (!is.null(row_split)) {
      visit_var   <- row_split
      visit_label <- cttab_get_label(dt[[row_split]], row_split)
      visit_val   <- as.character(agg[[row_split]])
    } else {
      # Empty strings (not NA) so callers can filter the report with plain
      # equality checks (`mis_rp$visit == "Baseline"`) without NA-induced
      # stray matches.
      visit_var <- ""
      visit_label <- ""
      visit_val <- ""
    }

    data.frame(
      form        = fm_name,
      visit_var   = visit_var,
      visit_label = visit_label,
      visit       = visit_val,
      variable    = v,
      label       = cttab_get_label(dt[[v]], v),
      missing_pct = sprintf("%s%% (%d/%d)",
                            round(100 * agg$n_miss / agg$n_total, 1),
                            agg$n_miss, agg$n_total),
      subject_id  = vapply(agg$subids, paste, character(1L), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, res_list)
  if (!is.null(res)) {
    # Append to a list and consolidate lazily in get_missing_report() —
    # an incremental rbind into cctu_env$missing_report_data was O(n^2)
    # over the lifetime of an analysis with hundreds of tables.
    cctu_env$missing_report_chunks <- c(
      cctu_env$missing_report_chunks,
      list(res)
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
  chunks <- cctu_env$missing_report_chunks
  if (length(chunks) == 0L) return(cctu_env$missing_report_data)
  combined <- rbind(cctu_env$missing_report_data,
                    do.call(rbind, chunks))
  combined <- unique(combined)
  cctu_env$missing_report_data <- combined
  cctu_env$missing_report_chunks <- list()
  combined
}

#' @rdname dump_missing_report
#' @export
reset_missing_report <- function() {
  cctu_env$missing_report_data <- empty_missing_report()
  cctu_env$missing_report_chunks <- list()
}

#' @keywords internal
empty_missing_report <- function() {
  setNames(
    data.frame(matrix(ncol = 8, nrow = 0)),
    c(
      "form", "visit_var", "visit_label",
      "visit", "variable", "label", "missing_pct",
      "subject_id"
    )
  )
}
