#' Merge vertically split data
#'
#' This function can be used when the MACRO dataset is \strong{vertically} split,
#' and must not be used to combine horizontally split data which has same
#' variable names. It will rename the variable of the dataset, including
#' shortcode of dlu and clu files with underscore and number, and combine them
#' to single dataset. Returns a list with merged dataset, DLU and CLU. Data, DLU
#' and CLU should be in same order. The merged dataset, dlu and clu can further
#' be passed to \code{\link{apply_macro_dict}}.
#'
#' @param datalist A list of dataset
#' @param dlulist A list of DLU data
#' @param clulist A list of CLU data
#'
#' @return A list of three data.frame
#' \itemize{
#'   \item{data}: Merged dataset.
#'   \item{dlu}: Merged DLU data.
#'   \item{clu}: Merged CLU data.
#' }
#' @export
#'
#' @seealso
#' \code{\link{read_data}}
#' \code{\link{apply_macro_dict}}
#'
#' @example inst/examples/merge-data.R
merge_data <- function(datalist,
                       dlulist,
                       clulist) {
  # Column names that is common across studies and datasets.
  com_cols <- c(
    "Trial", "Site", "Label", "PersonId", "VisitCycle",
    "FormCycle", "RepeatNumber"
  )

  for (i in seq_along(datalist)) {
    if (!all(com_cols %in% names(datalist[[i]]))) {
      stop(
        "The ", i, "th data in the datalist does not contain all the following variables: ",
        paste(com_cols, collapse = ", ")
      )
    }

    if (!all(dlulist[[i]][, 1] %in% names(datalist[[i]]))) {
      warning(
        "Not all the short code in the ", i,
        "th DLU can be found in the data ", i
      )
    }

    if (!all(clulist[[i]][, 1] %in% names(datalist[[i]]))) {
      warning(
        "Not all the short code in the ", i,
        "th CLU can be found in the data ", i
      )
    }

    nams <- setdiff(colnames(datalist[[i]]), com_cols)
    datalist[[i]] <- data.table::copy(datalist[[i]])
    data.table::setnames(datalist[[i]], nams, paste(nams, i, sep = "_"))

    # Rename DLU shortcode
    dlu_nams <- dlulist[[i]][dlulist[[i]][, 1] %in% nams, 1]
    dlulist[[i]][dlulist[[i]][, 1] %in% nams, 1] <- paste(dlu_nams, i,
      sep = "_"
    )

    # Rename CLU shortcode
    clu_nams <- clulist[[i]][clulist[[i]][, 1] %in% nams, 1]
    clulist[[i]][clulist[[i]][, 1] %in% nams, 1] <- paste(clu_nams, i,
      sep = "_"
    )
  }

  dlu <- unique(do.call(rbind, dlulist))
  clu <- unique(do.call(rbind, clulist))

  dt <- Reduce(
    function(df1, df2) merge(df1, df2, by = com_cols, all = TRUE, sort = FALSE),
    datalist
  )

  list(data = dt, dlu = dlu, clu = clu)
}
