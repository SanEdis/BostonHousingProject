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

### CONSTRUCTOR ###

### power_set: constructor function that generates a list for each element M_k, ###
### where each element is the subset with the highest R-squared value ###

power_set <- function(data, response_var) {
  
  set_summaries <- list()
  each_subset <- list()
  new_vec <- data.frame(rep(NA, nrow(data)))
  p <- ncol(data)
  
  powers <- subsets(p)
  
  for (i in 1:length(powers)) {
    for (j in 1:ncol(powers[[i]]) ) {
      each_rss <- rep(NA, ncol(powers[[i]]))
      each_summary <- list()
      
      for (k in 1:length(powers[[i]][,j]) ) {
        t = powers[[i]][k, j]
        vec <- data.frame(data[t])
        new_vec <- cbind(new_vec, vec) # generates a data frame of each subset within M_k
      }
      new_vec <- new_vec[-1] # eliminates NA list
      new_dta <- cbind(response_var, new_vec) # data frame of subset and the response 
      models <- suppressWarnings(summary(lm(response_var ~ ., data = new_dta)))
      # gets summary of attributes of each subset within M_k fit to a linear model
      each_rss[j] <- models[["r.squared"]][[1]] # creates a list of j lists for each R-squared of each k model
      each_subset[[j]] <- new_vec
      each_summary[[j]] <- models
    }
    max_subset_rss <- which.max(each_rss)
    
    set_summaries[[i]] <- list(each_subset[[max_subset_rss]] , each_summary[[max_subset_rss]])
  }
  na.omit(set_summaries)
  
  return(set_summaries) 
}

### SELECTORS ###


### set_selector : takes the output of the constructor function power_set and
### creates a list of the selected M_k subsets with the highest R-squared value ###

set_selector <- function(subset_list) {
  set <- lapply(subset_list, function(x) x[[1]])
  return(set)
}

### summary_selector : takes output of power_set function and selects summaries of each M_k subset
### with highest R-squared value ###


summary_selector <- function(subset_list) {
  summary <- lapply(subset_list, function(x) x[[2]])
  return(summary)
}

### var_selector : takes output of power_set function and selects variance of each M_k subset
### with highest R-squared value ###

var_selector <- function(subset_list) {
  var <- lapply(summary_selector(subset_list), function(x) x[["sigma"]]**2)
  return(var)
}








### index : indexes a set ###

index <- function(vec, value) {
  for (i in 1:length(vec)) {
    if (value == vec[i]) {
      idx <- i 
    }
  }
  return(idx)
}


