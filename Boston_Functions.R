### Functions File for Boston Housing Project ### 

## Prepared By : Samba Njie Jr
## Completed : 10/16/2016

# Best Subset Selection ---------------------------------------------------------------------------------

subsets <- function(p) { # finds the number of possible subsets of any sized set
  # p = ncol(data)
  stopifnot(is.numeric(p))
  total <- list()
  
  
  for (i in 1:p) {
    sets <- combn(p, i)
    total[[i]] <- sets
  }
  return(total)
}