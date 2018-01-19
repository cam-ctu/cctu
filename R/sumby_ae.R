#' Function to calculate summary statistics by group and by site for AEs/ADEs
#' @param safety_data  safety dataset - must have variables subjid, description, rx, siteid
#' @param rand_data    randomisation dataset - must have variable rx
#' @param varname     label for ae description - e.g. "ADE Description"
#' @param bysite      if TRUE then summaries calculated by site as well as treatment group
#'
#' @importFrom magrittr %>%
#' @return  a table with summary statistics in
#' @export

sumby_ae <- function(safety_data,
                     rand_data,
                     varname,
                     bysite = FALSE){

  if(bysite){
    sites   <- unique(safety_data$siteid)
    ans_all <- NULL

    for(i in 1:length(sites)){
      safety_sub <- safety_data[sites[i]==safety_data$siteid,]
      rand_sub   <- rand_data[sites[i]==rand_data$siteid,]

      df       <- unique(safety_sub[, c("subjid", "description", "rx")])
      tab      <- stats::xtabs(~ description + rx, data = df) %>% stats::addmargins
      dims     <- dim(tab)
      nams     <- dimnames(tab)
      var      <- rownames(tab)[-dims[1]]
      site     <- c(sites[i], rep("", dims[1] - 2))
      tab      <- matrix(tab[-dims[1], ], nrow = dims[1] - 1)
      total    <- stats::xtabs(~ rx, data = rand_sub) %>% stats::addmargins
      total    <- matrix(rep(total, dims[1] - 1), nrow = dims[1] - 1, byrow = T)
      perc     <- round(100 * tab / total, 1)
      X        <- array(0, dim = c(dim(perc), 3))
      X[, , 1] <- perc
      X[, , 2] <- tab
      X[, , 3] <- total

      perpast <- function(x){
        paste0(x[1], "% (", x[2], "/", x[3], ")")
      }

      value           <- apply(X, c(1, 2), perpast) %>%
        gsub("^0% ", "", .)        %>%
        gsub("^NaN% ", "", .)
      colnames(value) <- nams[[2]]

      ans                       <- data.frame(cbind(site, var, value), row.names = 1:(dims[1] - 1))
      colnames(ans)[c(1, 2, 5)] <- c("Site", varname, "Total")

      if(is.null(ans_all)){
        ans_all <- ans
      } else{
        ans_all <- rbind_space(ans_all, ans)
      }
    }
    ans_all <- apply(ans_all, 2, as.character)
    ans_all
  } else{
      df       <- unique(safety_data[, c("subjid", "description", "rx")])
      tab      <- stats::xtabs(~ description + rx, data = df) %>% stats::addmargins
      dims     <- dim(tab)
      nams     <- dimnames(tab)
      var      <- rownames(tab)[-dims[1]]
      tab      <- matrix(tab[-dims[1], ], nrow = dims[1] - 1)
      total    <- stats::xtabs(~ rx, data = rand_data) %>% stats::addmargins
      total    <- matrix(rep(total, dims[1] - 1), nrow = dims[1] - 1, byrow = T)
      perc     <- round(100 * tab / total, 1)
      X        <- array(0, dim = c(dim(perc), 3))
      X[, , 1] <- perc
      X[, , 2] <- tab
      X[, , 3] <- total

      perpast <- function(x){
        paste0(x[1], "% (", x[2], "/", x[3], ")")
      }

      value           <- apply(X, c(1, 2), perpast) %>%
        gsub("^0% ", "", .)        %>%
        gsub("^NaN% ", "", .)
      colnames(value) <- nams[[2]]

      ans                    <- data.frame(cbind(var, value), row.names = 1:(dims[1] - 1))
      colnames(ans)[c(1, 4)] <- c(varname, "Total")
      ans                    <- apply(ans, 2, as.character)
      ans
    }
}




















