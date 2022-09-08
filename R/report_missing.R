
#' Generate missing report
#'
#' This function use the same method as described in the \code{cctab}, reporting
#' missingness of the variables. It includes which form is the variable from,
#' set as `Derived` if not from DLU file. And missing percentage with which subjects
#' are have missing value for that particular variable. This is internal function,
#' not intend to use it directly by user.
#'
#' @inheritParams cttab
#' @param dlu A data.frame of DLU file, this will be derived from the package environment
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
                           select,
                           row_split = NULL,
                           subjid_string = getOption("cctu_subjid_string", default = "subjid")){

  blnk_miss <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                        c("form", "visit_var", "visit_label", "visit",
                          "variable", "label", "missing_pct", "subject_id"))

  dlu <- get_dlu()
  # If the subjid can not be found in the dataset
  if(!subjid_string %in% names(data))
    return(blnk_miss)

  # Generate selection vector function
  gen_selec <- function(dat, var, select = NULL) {
    if (is.null(select) | !var %in% names(select)) {
      return(rep(TRUE, length(dat[[var]])))
    } else{
      r <- eval(str2expression(select[var]), envir = dat)
      r & !is.na(r)
    }
  }

  vars <- unlist(vars, use.names = FALSE)

  # Check if missing
  any_miss <- sapply(vars, function(v)sum(is.na(data[[v]]))) > 0

  # If all missing, return blank
  if(all(!any_miss))
    return(blnk_miss)

  # Only report variables with missing
  vars <- vars[any_miss]

  res <- lapply(vars, function(v){
    variable <- ifelse(has.label(data[[v]]), var_lab(data[[v]]), v)

    z <- data[gen_selec(data, v, select[v]), ]

    subid <- z[[subjid_string]][is.na(z[[v]])]
    pct <- round(100*length(subid)/length(z[[subjid_string]]), 1)

    if(v %in% dlu$shortcode)
      fm_name <- dlu$form[dlu$shortcode == v]
    else
      fm_name <- "Derived"

    data.frame(form        = fm_name,
               visit_var   = NA,
               visit_label = NA,
               visit       = NA,
               variable    = v,
               label       = variable,
               missing_pct = paste0(pct, "% (",length(subid), "/",
                                    length(z[[subjid_string]]), ")"),
               subject_id  = paste(subid, collapse = ", "),
               row.names = NULL)
  })

  res <- do.call(rbind, res)
  res[res$subject_id != "", ]
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
#' \code{reset_missing_report} Reset the internal missingness report data to blank.
#'
#' @param x File path the report will be dumped to. Default is under `Output`
#' folder, named as `variable_missing_report.csv`.
#' @seealso \code{\link{cttab}} \code{\link{report_missing}}
#' @export
#'
dump_missing_report <- function(x = "Output/variable_missing_report.csv"){

  utils::write.csv(get_missing_report(),
                  file = x,
                  na = "", row.names = FALSE)
}

#' @rdname dump_missing_report
#' @export
get_missing_report <- function(){
  unique(cctu_env$missing_report_data)
}

#' @rdname dump_missing_report
#' @export
reset_missing_report <- function(){
  cctu_env$missing_report_data <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                                         c("form", "visit_var", "visit_label",
                                           "visit", "variable", "label", "missing_pct",
                                           "subject_id"))
}
