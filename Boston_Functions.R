### Functions File for Boston Housing Project ### 

## Prepared By : Samba Njie Jr
## Completed : 10/16/2016

# Best Subset Selection ---------------------------------------------------------------------------------


### subsets: finds the number of possible subsets of any sized set ###

subsets <- function(p) { 
  # p = ncol(data)
  stopifnot(is.numeric(p))
  total <- list()
  
  
  for (i in 1:p) {
    sets <- combn(p, i)
    total[[i]] <- sets
  }
  return(total)
}


### power_set: generates a list for each element M_k, ###
### where each element is the subset with the highest R-squared value ###


power_set <- function(data, response_var) {
  
  sets <- list() 
  each_subset <- list()
  new_vec <- data.frame(rep(NA, nrow(data)))
  p <- ncol(data)
  
  powers <- subsets(p)
  
  for (i in 1:length(powers)) {
    for (j in 1:ncol(powers[[i]]) ) {
      each_rss <- rep(NA, ncol(powers[[i]]))
      
      for (k in 1:length(powers[[i]][,j]) ) {
        t = powers[[i]][k, j]
        vec <- data.frame(data[t])
        new_vec <- cbind(new_vec, vec) # generates a data frame of each subset within M_k
      }
      new_vec <- new_vec[-1] # eliminates NA list
      new_dta <- cbind(response_var, new_vec) # data frame of subset and the response 
      models <- suppressWarnings(summary(lm(response_var ~ ., data = new_dta))[["r.squared"]][[1]]) 
      # gets R-squared value of each subset within M_k fit to a linear model
      each_rss[j] <- models # creates a list of j lists for each R-squared of each k model
      each_subset[[j]] <- new_vec
    }
    max_subset_rss <- which.max(each_rss)
    sets[[i]] <- each_subset[[max_subset_rss]] 
  }
  sets <- na.omit(sets)
  return(sets) 
}
