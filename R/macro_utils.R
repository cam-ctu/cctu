
#' Apply DLU/CLU file to the data frame
#'
#' The variable label attribute will be applied to the data frame from the DLU file.
#'
#' @param data Data frame to be applied.
#' @param dlu Data frame of DLU, see \code{\link{tidy_dlu}} for the requirements of DLU.
#' @param clu Data frame of CLU, see details for the requirements of CLU.
#' @param date_format Date format that will be tried by \code{\link[base]{as.POSIXct}},
#' default is \code{c("\%d/\%m/\%Y", "\%Y-\%m-\%d", "\%Y/\%m/\%d")}. You can add other formats you
#' may want to try. The conversion will be skipped if the formatting does not succeed.
#' @param clean_names Convert variable name to lower case (default), this will also change the
#' values in the DLU as well. See \code{\link{clean_names}} for details.
#' @param rm_empty Remove empty \code{"rows"}, \code{"cols"}, or \code{"both"} (default),
#'  or not \code{"none"}. The \code{\link{remove_blank_rows_cols}} function will be
#'  used to clean the empty rows and/or columns. If the data is large, this will
#'  take a long time, should be set to \code{"none"} in this case. Use
#' \code{options(cctu_rm_empty = "none")} to set global options.
#' @param check_catvar Check values of the category variable (defined in the DLU file)
#' contain any non-numeric values before converting variables to numeric.
#'
#' @details
#'
#' \subsection{Overview}{
#'
#' This function first convert the data to a \code{\link[data.table]{data.table}}.
#' This is to avoid the variable attributes dropped by base R functions. Then it will use
#' the dlu file to convert the data into corresponding variable types. After the conversion
#' of the data, variable and value label attribute will be created for the variable,
#' see \code{\link{var_lab}}) and \code{\link{val_lab}}.
#'
#' User can use \code{\link{lab2val}} to conver value labels of the data to values if the
#' value label is desired. If the \code{clean_names} is set to \code{TRUE}, the data name
#'  and the dl/clu will be cleaned, including the question names in the dlu. The cleaned dlu data
#' will be stored in the \code{cctu} environment. This will further be used by \code{\link{cttab}}
#' to populate the missing report, see \code{\link{report_missing}}. You can change this with
#' \code{\link{set_dlu}} function, but it will not have any effect on this function, see
#' \code{\link{set_dlu}} more details.
#'
#' Please use \code{\link{get_dlu}} to get the dlu cleaned by \code{apply_macro_dict} or use
#' \code{\link{tidy_dlu}} to clean it, which is the same function used by \code{apply_macro_dict}
#' to clean the DLU.
#' }
#'
#' \subsection{Variable conversion based on DLU type}{
#'
#' \itemize{
#'   \item{IntegerData}: Convert to numeric.
#'   \item{Real}: Convert to numeric.
#'   \item{Category}: If there are any non-numeric characters in the variable, no conversion
#' will be performed, otherwise convert to numeric.
#'   \item{Date}: Convert data date format with \code{\link[base]{as.POSIXct}}. The \code{date_format}
#' will be used to feed the \code{\link[base]{as.POSIXct}} function during the conversion.
#'   \item{Text}: Convert to character.
#' }
#' }
#'
#' \subsection{CLU data requirements}{
#'
#' The CLU file contains three columns:
#' \itemize{
#'   \item{ShortCode}: Variable name of the downloaded data.
#'   \item{CatCode}: Category values, it represents the numeric code for an item in the CRF.
#'   \item{CatValue}: Label of the category values, for example.
#' }
#' }
#' @seealso \code{\link{var_lab}} \code{\link{val_lab}} \code{\link{tidy_dlu}} \code{\link{set_dlu}}
#'  \code{\link[data.table]{data.table}} \code{\link{clean_names}} \code{\link{read_data}}
#'  \code{\link{remove_blank_rows_cols}} \code{\link{lab2val}} \code{\link{get_dlu}}
#' @return A data.table object.
#' @export
#'
#' @example inst/examples/apply_macro_dict.R
#'
apply_macro_dict <- function(data,
                             dlu,
                             clu = NULL,
                             date_format = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"),
                             clean_names = TRUE,
                             rm_empty = getOption("cctu_rm_empty", default = "both"),
                             check_catvar = FALSE){

  data <- data.table::as.data.table(data)

  rm_empty <- match.arg(rm_empty, choices = c("both", "none", "rows", "cols"))

  # Get name of the data.frame
  dlu_name <- deparse(substitute(dlu))

  dlu_var_list <- c("shortcode", "description", "type")
  colnames(dlu) <- tolower(colnames(dlu))
  if(!all(dlu_var_list %in% names(dlu)))
    stop("Variable ", paste(setdiff(dlu_var_list, names(dlu)), collapse = ", "),
         " not found in the dlu data.")

  if(!all(c("shortcode", "description", "type", "visit", "form", "question") %in% names(dlu)))
    dlu <- tidy_dlu(dlu, clean_names = clean_names)

  clu_var_list <- c("shortcode", "catcode", "catvalue")

  if(clean_names){
    colnames(data) <- clean_string(names(data))
    dlu$shortcode <- clean_string(dlu$shortcode)
    dlu$question <- clean_string(dlu$question)
  }

  # Store DLU file inside the cctu env
  cctu_env$dlu <- dlu

  if(!is.null(clu)){
    clu_name <- deparse(substitute(clu))
    colnames(clu) <- tolower(colnames(clu))
    if(!is.null(clu) && !all(clu_var_list %in% names(clu)))
      stop("Variable ", paste(setdiff(clu_var_list, names(clu)), collapse = ", "),
           " not found in the clu data.")

    if(clean_names)
      clu$shortcode <- clean_string(clu$shortcode)

  }

  # Keep the variables in the data only
  dlu <- dlu[dlu$shortcode %in% names(data), ]

  # Convert date
  date_cols <- dlu[dlu$type == "Date", "shortcode"]
  for (j in date_cols){

    #if(!all(unique(nchar(data[[j]])) %in% 10)){
    #  message(paste("Variable", j,"is not a valid date and will be skipped."))
    #  next
    #}

    skip_to_next <- FALSE

    val <- data[[j]]
    val[val == ""] <- NA
    has_time <- all(grepl(":", na.omit(val)))
    if(any(grepl("H|M|S|R|T|X|I|c", date_format)) & has_time)
      date_fn <- as.POSIXct
    else
      date_fn <- as.Date

    val <- tryCatch(date_fn(val, tryFormats = date_format),
                    error = function(e){
                      message(paste("An error occurred when converting variable", j,"to date and will be skipped:\n"),
                              conditionMessage(e))
                      skip_to_next <<- TRUE
                    },
                    warning = function(w) {
                      message(paste("A warning occurred when converting variable", j,"to date and will be skipped:\n"),
                              conditionMessage(w))
                      skip_to_next <<- TRUE
                    })
    if(skip_to_next) { next }

    data[[j]] <- val
    # set(data, j = j, value = val)
  }

  # Convert numeric
  num_cols1 <- dlu[dlu$type %in% c("IntegerData", "Real"), "shortcode"]
  num_cols2 <- dlu[dlu$type %in% c("Category"), "shortcode"]

  if(check_catvar)
    num_cols2 <- num_cols2[which(sapply(data[, num_cols2, with = FALSE], all_is_numeric))] # avoid none-numeric

  num_cols <- c(num_cols1, num_cols2)

  for (j in num_cols)
    set(data, j = j, value = as.numeric(data[[j]]))

  # Add variable label attributes
  col_list <- dlu$description
  names(col_list) <- dlu$shortcode
  invisible(lapply(names(col_list), function(x) setattr(data[[x]], "label", col_list[[x]])))

  # Add value labels
  if(!is.null(clu)){
    clu <- clu[clu$shortcode %in% names(data), ]
    clu_lst <- clu$catcode
    names(clu_lst) <- clu$catvalue

    if(any(is_empty(clu$catvalue)))
      stop("Variable ", paste(unique(clu$shortcode[is_empty(clu$catvalue)]), collapse = ", "),
           " has empty category value, please check.")

    clu_lst <- split(clu_lst, clu$shortcode)
    invisible(lapply(names(clu_lst), function(x) setattr(data[[x]], "labels", clu_lst[[x]])))
  }

  if(rm_empty != "none")
    data <- remove_blank_rows_cols(data, convert = FALSE, which = rm_empty)

  return(data)
}



#' Tidy DLU form
#'
#' @description Separate visit, form and question into different columns.
#' Variable names, NOT the values, of the dlu data will be converted to lower cases.
#' @param x DLU data
#' @param clean_names Conver variable name to lower case (default). See
#' \code{\link{clean_names}} for details.
#' @details
#' The DLU file contains four columns:
#' \itemize{
#'   \item{shortcode}: Variable name of the downloaded data.
#'   \item{visit/form/question}: Contains visit, form and question. It is
#' separated by a slash. This function will separate this column into `visit`,
#' `form` and `question` column in the output dataset. The `question` column
#' is the unique variable name for a particular CRF form.
#'   \item{description}: description of the variable, namely variable label.
#' This is will be used by \code{\link{apply_macro_dict}} to create variable label.
#'  \item{type}: type of the variable, it has IntegerData, Text, Date, Real and
#' Category four categories. This will be used by \code{\link{apply_macro_dict}} to
#' convert variables to corresponding type.
#' }
#' @return A data.frame with `visit/form/question` column separated into `visit`,
#' `form` and `question` column.
#' @seealso \code{\link{apply_macro_dict}} \code{\link{read_data}} \code{\link{cttab}}
#' \code{\link{report_missing}} \code{\link{clean_names}}
#' @export
#'
tidy_dlu <- function(x, clean_names = TRUE){

  if_visit_form <- unique(sapply((gregexpr("/", x[[2]], fixed=TRUE)), function(i) sum(i > 0)))
  if(length(if_visit_form) != 1 || if_visit_form != 2)
    stop("The second variable of the DLU file must be in the original visit/form/question from MACRO.")

  vfq <- strsplit(as.character(x[[2]]),'/')
  vfq <- as.data.frame(do.call(rbind, vfq))
  colnames(vfq) <- c("visit", "form", "question")
  colnames(x) <- tolower(colnames(x))

  dlu <- cbind.data.frame(x[, -2], vfq)
  if(clean_names){
    dlu$shortcode <- clean_string(dlu$shortcode)
    dlu$question <- clean_string(dlu$question)
  }

  return(dlu)
}


#' @name set_dlu
#' @aliases get_dlu
#' @title Set/get DLU data
#'
#' @description
#' \code{set_dlu} will set the provided DLU data to package environment, so it can be used
#'  for missing data report by \code{\link{cttab}}. It is user's responsibility to make
#'  sure values of the short code in the provided DLU data matches the variable names in
#'  the dataset. \code{set_dlu} will not have any effect on \code{\link{apply_macro_dict}}.
#' Instead, the \code{\link{apply_macro_dict}} will override the `DLU` seetings done by
#' \code{set_dlu}.
#'
#' \code{get_dlu} can be used to get a copy of DLU data in stored by \code{set_dlu}.
#'
#' @inheritParams tidy_dlu
#' @export
set_dlu <- function(x, clean_names = TRUE){
  colnames(x) <- tolower(colnames(x))
  if(!all(c("shortcode", "description", "type", "visit", "form", "question") %in% names(x)))
    x <- tidy_dlu(x, clean_names)

  # Store DLU file inside the cctu env
  cctu_env$dlu <- x
  invisible(x)
}

#' @rdname set_dlu
#' @export
get_dlu <- function(){
  cctu_env$dlu
}


#' Extract data by form from MACRO dataset
#'
#' @description Extract data by form from MACRO dataset. Data will be transformed to long format
#'  adding a new column of `visit`.
#'
#' @param data A data.frame from macro dataset.
#' @param form Name of the form in the DLU file, see \code{\link{tidy_dlu}}.
#' @param dlu A DLU data.frame
#' @param visit A character string or vector of visit name in the DLU file, see \code{\link{tidy_dlu}}.
#' @param vars_keep Parameters to keep in the output data. This is useful if you want to keep
#' treatment arm or age variable.
#' @param rm_empty Remove empty \code{"rows"}, \code{"cols"}, or \code{"both"} (default),
#'  or not \code{"none"}. The \code{\link{remove_blank_rows_cols}} function will be
#'  used to clean the empty rows and/or columns. Use \code{options(cctu_rm_empty = "none")}
#' to set global options.
#' @seealso \code{\link{tidy_dlu}} \code{\link[data.table]{data.table}}
#' \code{\link{read_data}} \code{\link{remove_blank_rows_cols}}
#' @export
#'
#' @example inst/examples/apply_macro_dict.R
#'
#' @return A data.table object.
#'
#' @importFrom stats setNames
#' @import data.table
#'
#'
extract_form <- function(data,
                         form,
                         visit = NULL,
                         vars_keep = NULL,
                         dlu = cctu_env$dlu,
                         rm_empty = getOption("cctu_rm_empty", default = "both")){

  rm_empty <- match.arg(rm_empty, choices = c("both", "none", "rows", "cols"))

  if(ncol(dlu) == 4 & names(dlu)[2] == "visit.form.question")
    dlu <- tidy_dlu(dlu)

  if(length(form) > 1)
    stop("Form must be of length 1.")

  if(!form %in% dlu$form)
    stop("Form name ", form, " can not be found in the DLU file.")

  dlu <- dlu[dlu$form == form, ]

  if(!is.null(visit)){
    if(!all(visit %in% dlu$visit))
      stop("Visit name ", paste(setdiff(visit, dlu$visit), collapse = ", "), " can not be found in the DLU file.")

    dlu <- dlu[dlu$visit %in% visit, ]
  }


  data <- data.table::as.data.table(data)

  res <- lapply(unique(dlu$visit), function(v){
    vars_selc <- dlu$shortcode[dlu$visit == v]
    dt <- data[, c(vars_keep, vars_selc), with = FALSE]
    colnames(dt) <- c(vars_keep, dlu$question[dlu$visit == v])

    # If the number of missing column equals to the number of variables
    dt$if.all.miss <- rowSums(apply(dt[, dlu$question[dlu$visit == v], with = FALSE],
                                    2,
                             function(x)x %in% c(NA, ""))) == length(vars_selc)
    dt$form_visit <- v
    dt
  })

  res <- data.table::rbindlist(res, use.names=TRUE)
  res <- res[!res$if.all.miss, ]
  res$if.all.miss <- NULL

  if(rm_empty != "none")
    res <- remove_blank_rows_cols(res, convert = FALSE, which = rm_empty)

  return(data.table::as.data.table(res))
}


