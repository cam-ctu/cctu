

#' Generate an table of descriptive statistics.
#'
#' This is a wrapper function of \code{stat_tab}, allowing for groupped variables,
#' split statistics table by `row_split` variable.
#'
#' @param vars Variables to be used for summary table.
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
#'   reported in the summary table, default is `TRUE`.
#' @param add_obs Add an observation row (default).
#' @param dlu A data.frame of DLU file.
#' @param subjid_string A character naming the column used to identify subject.
#'
#' @return A matrix with `cttab` class.
#' @export
#'
cttab <- function(vars,
                 data,
                 group = NULL,
                 row_split = NULL,
                 total = TRUE,
                 select = NULL,
                 add_missing = TRUE,
                 add_obs = TRUE,
                 dlu = cctu_env$dlu,
                 subjid_string = "subjid") {

  vars_list <- c(unlist(vars), group, row_split)
  if (!all(vars_list %in% names(data))) {
    stop(
      "Variable ",
      paste(vars_list[!vars_list %in% names(data)], collapse = ", "),
      " not in the dataset, please check!"
    )
  }

  # Convert to data.table to avoid format lose.
  data.table::setDT(data)

  # Group variable to factor
  if (!is.null(group)) {

    # Remove missing records for group
    data <- data[!is.na(data[[group]]), ]

    if (has.labels(data[[group]]) | !is.factor(data[[group]]))
      data[[group]] <- to_factor(data[[group]], drop.levels = TRUE)
  }

  if(base::anyDuplicated(vars_list))
    stop("vars, group and row_splot duplicated.")

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
                      add_missing = add_missing)

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
                      add_missing = add_missing)
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

  # If no split
  if (is.null(row_split)) {
    tbody <- calc_tab(data)

    # Report missing
    if(!is.null(dlu) && !is.null(subjid_string)){
      miss_rep <- report_missing(data = data, vars = vars, select = select,
                               dlu = dlu, subjid_string = subjid_string)

    
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
      if(!is.null(dlu) && !is.null(subjid_string)){
        miss_rep <- report_missing(data = dfm[[x]], vars = vars, select = select,
                                  dlu = dlu, subjid_string = subjid_string)
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
#' @param vars Variables to be used for summary table.
#' @param data A \code{data.frame} from which the variables in \code{vars}
#' should be taken.
#' @param group Name of the grouping variable.
#' @param total If a "Total" column will be created (default). Specify
#' \code{FALSE} to omit the column.
#' @param select a named vector with as many components as row-variables. Every
#' element of `select` will be used to select the individuals to be analyzed
#'  for every row-variable. Name of the vector corresponds to the row variable,
#'  element is the selection.
#' @param add_missing If missing number and missing percentage in the  will be
#'   reported in the summary table, default is `TRUE`.
#'
#' @return An object of class "cttab".
#'
#' @examples
#'
#' dat <- expand.grid(id=1:10, sex=c("Male", "Female"), treat=c("Treated", "Placebo"))
#' dat$age <- runif(nrow(dat), 10, 50)
#' dat$age[3] <- NA  # Add a missing value
#' dat$wt <- exp(rnorm(nrow(dat), log(70), 0.2))
#'
#' var_lab(dat$sex) <- "Sex"
#' var_lab(dat$age) <- "Age"
#' var_lab(dat$treat) <- "Treatment Group"
#' var_lab(dat$wt) <- "Weight"
#'
#'
#' # Something more complicated
#'
#' dat$dose <- ifelse(dat$treat=="Placebo", "Placebo",
#'                    sample(c("5 mg", "10 mg"), nrow(dat), replace=TRUE))
#' dat$dose <- factor(dat$dose, levels=c("Placebo", "5 mg", "10 mg"))
#'
#'
#' cttab(c("age", "sex", "wt"),
#' data = dat,
#' group = "treat")
#' @importFrom data.table .SD
#' @keywords utilities
#' @export

stat_tab <- function(vars,
                     group = NULL,
                     data,
                     total  = TRUE,
                     select = NULL,
                     add_missing = TRUE){

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

  # Generate selection vector function
  gen_selec <- function(dat, var, select = NULL) {
    if (is.null(select) | !var %in% names(select)) {
      return(rep(TRUE, length(dat[[var]])))
    } else{
      r <- eval(str2expression(select[var]), envir = dat)
      r & !is.na(r)
    }
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
        stop(paste("Variable", v, "are", class(z), "and not supported!"))

      if(inherits(z, c("factor", "character"))){
        r <- c("", render_cat(z))
      }

      if(inherits(z, "logical")){
        r <- miss <- with(cat_stat(z)$Yes,
                          c(Missing = ifelse(FREQ == 0, "",
                                             sprintf("%s/%s (%s)", FREQ, Nall, PCT))))
        add_missing <- FALSE
      }

      if(inherits(z, c("numeric", "integer"))){
        r <- c("", render_numeric(z))
      }

      names(r)[1] <- variable

      if(add_missing & any_miss[v]){
        miss <- with(cat_stat(is.na(z))$Yes,
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

    if(nrow(y) == 1 & all(y == ""))
      return(NULL)

    fst <- ifelse(nrow(y) == 1, 3, 2)

    structure(y,
              position = c(fst, rep(3, nrow(y) - 1)),
              class = c("cttab", class(y)))
  }))

  return(r)

}

