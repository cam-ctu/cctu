#'Produces summary statistics on a variable broken down by arm, display a graphical equivalent.
#'
#'@inheritParams sumfig
#'@param total logical value to include an extra overall columns. Defaults to true
#'@param fig logical value on whether to print (if interactive) and save a copy of the figure. Defaults to true
#'@param directory the path to the directory where figures will be saved as "sumby_XX_Y.png". XX is taken from the current table numer (or "0") set in \code{\link{attach_pop}}, and the Y is counting how many time \code{\link{sumby}} has been run since the XX was last set.
#' @param verbose logical to print information on changes to the global environment or external files. Defaults to options()$verbose.
#'@param text_clean a function to transform character labels. Defaults to propercase. Or set to NULL if you want to preserve the original text.
#'@param pct_digits number of decimal places to present percentages to. Defaults to 0.
#'
#'@return a data.frame containing summary statistics in character format,
#'ready to use with write_table(). Plus an attribute "fig" that contains a ggplot object
#'
#'
#'@seealso \code{\link{sumfig}} , \code{\link{write_table}}, \code{\link{propercase}}
#'
#'@importFrom magrittr %<>% %>%
#'@export


sumby <- function(variable,
                  arm,
                  label = NULL,
                  data  = parent.frame(),
                  total = TRUE,
                  fig   = TRUE,
                  directory=file.path("Output","Figures"),
                  verbose=options()$verbose,
                  text_clean=propercase,
                  pct_digits=0
                  ){

  variable_name <- deparse(substitute(variable))
  if(is.null(label)){label <- variable_name}
  mf       <- match.call()
  variable <- eval(mf$variable, envir = data)
  arm      <- eval(mf$arm, envir = data)



  if(fig){
    fig_object <- sumfig(variable = variable, arm = arm, label = label)
    if( interactive()){ print(fig_object) }
    cctu_env$sumby_count <- cctu_env$sumby_count + 1
    if( directory!=formals()$directory | cctu_check_dir()){
      #directory %<>% normalizePath %>% final_slash
      file_name <- file.path(directory,paste0("sumby_fig_",cctu_env$number,"_", cctu_env$sumby_count,".png"))
      ggsave(file_name, fig_object)
      if(verbose){cat(file_name, "created.\n")}
   }
  }


  if(total){
    variable      <- rep(variable, 2)
    arm_character <- c(as.character(arm), rep("Total", length(arm)))
    if(is.factor(arm)){
      arm_levels <- c(levels(arm), "Total")
      arm        <- factor(arm_character, levels = arm_levels)
    } else{
      arm <- arm_character
    }
  }

  if(is.null(text_clean)){text_clean <- function(x){ as.character(x) }}

  variable.class = class(variable)
  # continuous variable summary statistics by arm
  if(inherits(variable, "numeric") || inherits(variable,"integer")){
    mu   = format(tapply(variable, arm, mean, na.rm = T), digits = 3, width = 2)
    sd   = format(tapply(variable, arm, sd, na.rm = T), digits = 3, width = 2)
    n    = tapply(variable, arm, function(x){sum(!is.na(x))})
    meds = tapply(variable, arm, stats::median, na.rm = T)
    mins = tapply(variable, arm, min, na.rm = T)
    maxs = tapply(variable, arm, max, na.rm = T)
    variable = c(text_clean(label), "", "", "")
    stats = c("n", "Mean (SD)", "Median", "Min, Max")
    value = rbind(n, paste0(mu," (", sd, ")"), meds, paste0(mins, ", ", maxs))
    ans   = data.frame(cbind(variable, stats, value), row.names = 1:4)
  }
  # categorical variable summary statistics by arm
  if(inherits(variable, "factor") || variable.class == "character"){
    tab   = table(variable, arm)
    dims  = dim(tab)
    nams  = dimnames(tab)
    tab   = as.data.frame(tab)
    total = matrix(rep(with(tab, tapply(Freq, arm, sum)), dims[1]), nrow = dims[1], byrow = T)
    tab   = stats::reshape(tab, direction = "wide", v.names = "Freq", timevar = "arm", idvar = "variable")
    variable = c(text_clean(label), rep("", dims[1]-1))
    stats = text_clean(tab[, 1])
    tab   = tab[, -1]
    perc  = round(100 * tab / total, digits=pct_digits)

    X        = array(0, dim = c(dim(perc), 3))
    X[, , 1] = as.matrix(perc)
    X[, , 2] = as.matrix(tab)
    X[, , 3] = as.matrix(total)

    perpast = function(x){
      paste0( x[2], "/", x[3], " (",x[1],  "%)")
    }
    value           = apply(X, c(1, 2), perpast)
    colnames(value) = nams[[2]]

    ans = data.frame(cbind(variable, stats, value), row.names = 1:dims[1])
  }

  names(ans)[1:2]  <- c("Variable", "Statistics")
  ans %<>%
    apply(., 2, as.character) %>%
    gsub("^0% ", "", .)       %>%
    gsub("^NaN% ", "", .)

  if(fig) attr(ans, "fig") <- fig_object
  ans
}
