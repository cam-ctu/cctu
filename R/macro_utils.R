
#' Apply DLU/CLU file to the data frame
#'
#' The variable label attribute will be applied to the data frame from the DLU file.
#'
#' @param data Data frame to be applied.
#' @param dlu Data frame of DLU
#' @param clu Data frame of CLU
#' @param date_format Date format to be converted, default is `\%d/\%m/\%Y`.
#' @param clean_names Conver variable name to lower case (default), this will also change the
#' values in the DLU as well. See \code{\link{clean_names}} for details.
#' @param rm_empty Remove empty \code{"rows"}, \code{"cols"}, or \code{"both"} (default),
#'  or not \code{"none"}. The \code{\link{remove_blank_rows_cols}} function will be
#'  used to clean the empty rows and/or columns. If the data is large, this will
#'  take a long time, should be set to \code{"none"} in this case. Use
#' \code{options(cctu_rm_empty = "none")} to set global options.
#' @details This function first convert the data to a \code{\link[data.table]{data.table}}.
#' This is to avoid the variable attributes dropped by base R functions. Then it will use
#' the dlu file to convert the data into corresponding variable types.
#' \itemize{
#'   \item{IntegerData}: Convert to numeric.
#'   \item{Real}: Convert to numeric.
#'   \item{Category}: If there are any non-numeric characters in the variable, no conversion
#' will be performed, otherwise convert to numeric.
#'   \item{Date}: Convert data date format with \code{\link{as.Date}}. The \code{date_format}
#' will be used to feed the \code{\link{as.Date}} function during the conversion.
#'   \item{Text}: Convert to character.
#' }
#' After the conversion of the data, variable label attribute will be created for the variable.
#' See  \code{\link{var_lab}}.
#'
#' If the \code{clean_names} is set to \code{TRUE}, the data name and the dl/clu
#' will be cleaned, including the question names in the dlu. The cleaned dlu data
#' will be stored in the \code{cctu} environment.
#'
#' After this is completed and if the clu file is provided, value label attribute will be
#' create for the variables listed in the clu file. See \code{\link{val_lab}}.
#' @seealso \code{\link{var_lab}} \code{\link{val_lab}} \code{\link{sep_dlu}}
#'  \code{\link[data.table]{data.table}} \code{\link{clean_names}} \code{\link{read_data}}
#'  \code{\link{remove_blank_rows_cols}}
#' @return A data.table object.
#' @export
#'
apply_macro_dict <- function(data,
                             dlu,
                             clu = NULL,
                             date_format = "%d/%m/%Y",
                             clean_names = TRUE,
                             rm_empty = getOption("cctu_rm_empty", default = "both")){

  #data.table::setDT(data)
  data <- data.table::as.data.table(data)

  rm_empty <- match.arg(rm_empty, choices = c("both", "none", "rows", "cols"))

  # Get name of the data.frame
  dlu_name <- deparse(substitute(dlu))
  clu_name <- deparse(substitute(clu))

  dlu_var_list <- c("ShortCode", "Description", "Type")
  if(!all(dlu_var_list %in% names(dlu)))
    stop("Variable ", paste(setdiff(dlu_var_list, names(dlu)), collapse = ", "),
         " not found in the dlu data. Please DO NOT clean DLU file.")

  if(ncol(dlu) == 4)
    dlu <- sep_dlu(dlu)

  clu_var_list <- c("ShortCode", "CatCode", "CatValue")
  if(!is.null(clu) && !all(names(clu) %in% clu_var_list))
    stop("Variable ", paste(setdiff(clu_var_list, names(clu)), collapse = ", "),
         " not found in the clu data. Please DO NOT clean CLU file.")

  if(clean_names){
    colnames(data) <- clean_string(names(data))
    dlu$ShortCode <- clean_string(dlu$ShortCode)
    dlu$Question <- clean_string(dlu$Question)
    if(!is.null(clu))
      clu$ShortCode <- clean_string(clu$ShortCode)
  }

  # Store DLU file inside the cctu env
  cctu_env$dlu <- dlu

  # Restore the dlu and clu to parent frame
  assign(dlu_name, dlu, envir = parent.frame())
  message(dlu_name, " modified with ShortCode and Question cleaned.")
  if(!is.null(clu)){
    assign(clu_name, clu, envir = parent.frame())
    message(clu_name, " modified with ShortCode cleaned.")
  }

  # Keep the variables in the data only
  dlu <- dlu[dlu$ShortCode %in% names(data), ]

  # Convert date
  date_cols <- dlu[dlu$Type == "Date", "ShortCode"]
  for (j in date_cols)
    set(data, j = j, value = as.Date(data[[j]], date_format))

  # Convert numeric
  num_cols1 <- dlu[dlu$Type %in% c("IntegerData", "Real"), "ShortCode"]
  num_cols2 <- dlu[dlu$Type %in% c("Category"), "ShortCode"]
  num_cols2 <- num_cols2[which(sapply(data[, num_cols2, with = FALSE], all_is_numeric))] # avoid none-numeric
  num_cols <- c(num_cols1, num_cols2)

  for (j in num_cols)
    set(data, j = j, value = as.numeric(data[[j]]))

  # Convert characters
  text_cols <- dlu[dlu$Type == "Text", "ShortCode"]
  for (j in text_cols)
    set(data, j = j, value = as.character(data[[j]]))

  # Add variable label attributes
  col_list <- dlu$Description
  names(col_list) <- dlu$ShortCode
  invisible(lapply(names(col_list), function(x) setattr(data[[x]], "label", col_list[[x]])))

  # Add value labels
  if(!is.null(clu)){
    clu <- clu[clu$ShortCode %in% names(data), ]
    clu_lst <- clu$CatCode
    names(clu_lst) <- clu$CatValue

    if(any(is_empty(clu$CatValue)))
      stop("Variable ", paste(unique(clu$ShortCode[is_empty(clu$CatValue)]), collapse = ", "),
           " has empty category value, please check.")

    clu_lst <- split(clu_lst, clu$ShortCode)
    invisible(lapply(names(clu_lst), function(x) setattr(data[[x]], "labels", clu_lst[[x]])))
  }

  if(rm_empty != "none")
    data <- remove_blank_rows_cols(data, convert = FALSE, which = rm_empty)

  return(data)
}



#' Tidy DLU form
#'
#' @description Separate Visit, Form and Question into different columns.
#' The DLU file contains four columns:
#' \itemize{
#'   \item{ShortCode}: Variable name of the downloaded data.
#'   \item{Visit/Form/Question}: Contains visit, form and question. It is
#' separated by a slash. This function will separate this column into `Visit`,
#' `Form` and `Question` column in the output dataset. The `Question` column
#' is the unique variable name for a particular CRF form.
#'   \item{Description}: Description of the variable, namely variable label.
#' This is will be used by \code{\link{apply_macro_dict}} to create variable label.
#'  \item{Type}: Type of the variable, it has IntegerData, Text, Date, Real and
#' Category four categories. This will be used by \code{\link{apply_macro_dict}} to
#' convert variables to corresponding type.
#' }
#' @param x DLU data.frame
#' @return A data.frame
#' @seealso \code{\link{apply_macro_dict}} \code{\link{read_data}}
#' @export
#'
sep_dlu <- function(x){

  if_visit_form <- unique(sapply((gregexpr("/", x[[2]], fixed=TRUE)), function(i) sum(i > 0)))
  if(length(if_visit_form) != 1 || if_visit_form != 2)
    stop("The second variable of the DLU file must be in the original Visit/Form/Question from MACRO.")

  vfq <- strsplit(as.character(x[[2]]),'/')
  vfq <- as.data.frame(do.call(rbind, vfq))
  colnames(vfq) <- c("Visit", "Form", "Question")
  cbind.data.frame(x[, -2], vfq)
}

#' Extract data by form from MACRO dataset
#'
#' @description Extract data by form from MACRO dataset. Data will be transformed to long format
#'  adding a new column of `Visit`.
#'
#' @param data A data.frame from macro dataset.
#' @param form Name of the form in the DLU file, see \code{\link{sep_dlu}}.
#' @param dlu A DLU data.frame
#' @param visit A character string or vector of visit name in the DLU file, see \code{\link{sep_dlu}}.
#' @param vars_keep Parameters to keep in the output data. This is useful if you want to keep
#' treatment arm or age variable.
#' @param rm_empty Remove empty \code{"rows"}, \code{"cols"}, or \code{"both"} (default),
#'  or not \code{"none"}. The \code{\link{remove_blank_rows_cols}} function will be
#'  used to clean the empty rows and/or columns. Use \code{options(cctu_rm_empty = "none")}
#' to set global options.
#' @seealso \code{\link{sep_dlu}} \code{\link[data.table]{data.table}}
#' \code{\link{read_data}} \code{\link{remove_blank_rows_cols}}
#' @export
#'
#' @examples
#' \dontrun{
#' extract_form(full_dt, "LabResF", dlu)
#' }
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

  if(ncol(dlu) == 4 & names(dlu)[2] == "Visit.Form.Question")
    dlu <- sep_dlu(dlu)

  if(length(form) > 1)
    stop("Form must be of length 1.")

  if(!form %in% dlu$Form)
    stop("Form name ", form, " can not be found in the DLU file.")

  dlu <- dlu[dlu$Form == form, ]

  if(!is.null(visit)){
    if(!all(visit %in% dlu$Visit))
      stop("Visit name ", paste(setdiff(visit, dlu$Visit), collapse = ", "), " can not be found in the DLU file.")

    dlu <- dlu[dlu$Visit %in% visit, ]
  }


  #data.table::setDT(data)
data <- data.table::as.data.table(data)

  res <- lapply(unique(dlu$Visit), function(v){
    vars_selc <- dlu$ShortCode[dlu$Visit == v]
    dt <- data[, c(vars_keep, vars_selc), with = FALSE]
    colnames(dt) <- c(vars_keep, dlu$Question[dlu$Visit == v])

    # If the number of missing column equals to the number of variables
    dt$if.all.miss <- rowSums(apply(dt[, dlu$Question[dlu$Visit == v], with = FALSE],
                                    2,
                             function(x)x %in% c(NA, ""))) == length(vars_selc)
    dt$FormVisit <- v
    dt
  })

  res <- data.table::rbindlist(res, use.names=TRUE)
  res <- res[!res$if.all.miss, ]
  res$if.all.miss <- NULL

  if(rm_empty != "none")
    res <- remove_blank_rows_cols(res, convert = FALSE, which = rm_empty)

  #return(data.table::setDT(res))
  return(data.table::as.data.table(res))
}


