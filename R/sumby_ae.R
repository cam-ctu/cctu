#### Function to calculate summary statistics by group and by site for AEs/ADEs
PROGNAME <- "sumby_ae.R"
#### Author: Annabel Allison
#### Study: ReACt
#### DMC report Sep 2017
#### Date created: 21JUL2017
#### Notes: 
#### - removes 0% and NaN%

# safety_data = safety dataset - must have variables subjid, description, rx, siteid
# rand_data   = randomisation dataset - must have variable rx
# varname     = label for ae description - e.g. "ADE Description"
# bysite      = if TRUE then summaries calculated by site as well as treatment group
sumby_ae <- function(safety_data, 
                     rand_data, 
                     varname, 
                     bysite = FALSE){
  
  if(bysite){
    sites   <- unique(safety_data$siteid)
    ans_all <- NULL
    
    for(i in 1:length(sites)){
      safety_sub <- subset(safety_data, siteid == sites[i])
      rand_sub   <- subset(rand_data, siteid == sites[i])  
      
      df       <- unique(safety_sub[, c("subjid", "description", "rx")])
      tab      <- xtabs(~ description + rx, data = df) %>% addmargins
      dims     <- dim(tab)
      nams     <- dimnames(tab)
      var      <- rownames(tab)[-dims[1]]
      site     <- c(sites[i], rep("", dims[1] - 2))
      tab      <- matrix(tab[-dims[1], ], nrow = dims[1] - 1)
      total    <- xtabs(~ rx, data = rand_sub) %>% addmargins
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
      tab      <- xtabs(~ description + rx, data = df) %>% addmargins
      dims     <- dim(tab)
      nams     <- dimnames(tab)
      var      <- rownames(tab)[-dims[1]]
      tab      <- matrix(tab[-dims[1], ], nrow = dims[1] - 1)
      total    <- xtabs(~ rx, data = rand_data) %>% addmargins
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













  
  
  




