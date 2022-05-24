
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
#' @details This funciton first convert the data to a \code{\link[data.table]{data.table}}.
#' This is to avoid the variable attributes dropped by base R functions. Then it will use
#' the dlu file to convert the data into corresponding variable types.
#' \itemize{
#'   \item{IntegerData}: Convert to numeric.
#'   \item{Real}: Convert to numeric.
#'   \item{Category}: If there are any non-numerica characters in the variable, no conversion
#' will be performed, otherwise convert to numeric.
#'   \item{Date}: Convert data date format with \code{\link{as.Date}}. The \code{date_format}
#' will be used to feed the \code{\link{as.Date}} function during the covnersion.
#'   \item{Text}: Convert to character.
#' }
#' After the conversion of the data, variable label attribute will be created for the variable.
#' See  \code{\link{var_lab}}.
#'
#' After this is completed and if the clu file is provided, value label attribute will be
#' create for the variables listed in the clu file. See \code{\link{val_lab}}.
#' @seealso \code{\link{var_lab}} \code{\link{val_lab}} \code{\link{sep_dlu}}
#'  \code{\link[data.table]{data.table}} \code{\link{clean_names}} \code{\link{read_data}}
#' @return A data.table object.
#' @export
#'
apply_macro_dict <- function(data, dlu, clu = NULL, date_format = "%d/%m/%Y", clean_names = TRUE){

  data.table::setDT(data)

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

  # Keep the variables in the data only
  dlu <- dlu[dlu$ShortCode %in% names(data), ]

  for (i in dlu$ShortCode) {
    # Format date
    if (dlu$Type[dlu$ShortCode == i] == "Date")
      data[[i]] <- as.Date(data[[i]], date_format)

    # Format Number
    if (dlu$Type[dlu$ShortCode == i] %in% c("IntegerData", "Real", "Category") && all_is_numeric(data[[i]]))
        data[[i]] <- as.numeric(data[[i]])

    # Format date
    if (dlu$Type[dlu$ShortCode == i] == "Text")
      data[[i]] <- as.character(data[[i]])

    # Assign label
    var_lab(data[[i]]) <- dlu$Description[dlu$ShortCode == i]

    if(!is.null(clu) && i %in% clu$ShortCode){
      valab <- clu[clu$ShortCode == i, "CatCode"]
      names(valab) <- clu[clu$ShortCode == i, "CatValue"]

      if(any(is_empty(names(valab))))
        stop("Variable ", i, " has empty category values.")

      val_lab(data[[i]]) <- valab
    }

  }

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
#' @seealso \code{\link{sep_dlu}} \code{\link[data.table]{data.table}} \code{\link{read_data}}
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
extract_form <- function(data, form, visit = NULL, vars_keep = NULL, dlu = cctu_env$dlu){
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


  data.table::setDT(data)

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

  return(data.table::setDT(res))
}


