#' create environments contain a set of data frames filtered for each population
#'
#' @param data_names a character vector of the key data frames to be filtered
#' @param popn a data frame one-row per participant  a subjid variable and a
#' column for each population indicating if a participant belongs to the
#' population. The names of the variables must match up to the values in
#' meta_table$Population.
#' @param subjid_string the character naming the column used in each data frame
#'  from data_names and popn, to identify the subjid.
#' @param rm_from_frame a logical indicating if the data sets should be removed
#' from the frame environment
#' @param frame the frame in which the original list of data sets are found,
#' and  in which the new environments will be contained.
#' @param verbose logical to print information on changes to the global
#' environment or external files. Defaults to options()$verbose.
#'
#' @details  Now to use the data frames defined above in data_names you have to
#'  either directly call attach(safety), for example or look-up with
#'  attach_pop(1.01) for a table number. There are separate R environments made
#'   for each of the populations in population & population_title be careful to
#'    use detach(safety) - this is automated with write_table() and
#'    write_ggplot()
#'
#' @export


create_popn_envir <- function(data_names,
                              popn,
                              subjid_string = "subjid",
                              rm_from_frame = TRUE,
                              frame = parent.frame(),
                              verbose = options()$verbose) {
  popn_names <- names(popn)[names(popn) != subjid_string]

  for (pop_name in popn_names) {
    env_temp <- new.env(parent = frame)
    # work through the columns of population, and create environments for all
    for (df in data_names) {
      data_temp <- eval(as.name(df), envir = frame)
      # important here to automate - depends on consistent use of subjid
      # some data frames might not have subjid ...
      popn_temp <- popn[[subjid_string]][popn[[pop_name]]]
      # take a subset based on the logical variable in population
      data_temp <- data_temp[data_temp[[subjid_string]] %in% popn_temp, ]
      assign(df, data_temp, envir = env_temp)
    }
    # keep an environment of the different population-based version
    # of the data sets
    assign(pop_name, env_temp, envir = frame)
    if (verbose) {
      cat(pop_name, "environment created in", environmentName(frame), "\n")
    }
    # rm(env_temp, data_temp, population_temp)
  }

  # remove the original data frames from the search path - else you will
  # get duplications


  if (rm_from_frame) {
    if (verbose) {
      cat(
        "data frames removed from", environmentName(frame), ":",
        data_names, "\n"
      )
    }
    rm(list = data_names, envir = frame)
  }
  # rm(data_names, row, df)
}
