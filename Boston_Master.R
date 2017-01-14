### Boston Housing Project ###

## Prepared By : Samba Njie Jr
## Completed : 10/16/2016

# Libraries ------------------------------------------------------------------------------------------

cat("### Loading libraries... ###" )
library(ISLR)
library(MASS)
library(dplyr)
library(utils)
library(corrplot)
library(glmnet)

# Source In Functions -----------------------------------------------------------------------------------

cat("### Loading User-Defined Functions... ###" )




# Reading Data ------------------------------------------------------------------------------------------

dataset <- read.csv(file = "/Users/sambamamba/Documents/Cal Fall 2016/Stat 154/BostonHousingProject/housing.csv")
head(dataset) #inspect the columns and rows in the beginning

cat("### Establishing Training and Test Sets... ###")

set.seed(1)
n_obs = nrow(dataset) #number of observations
n_features = ncol(dataset) #number of features

prop_training = round(0.90*n_obs) #subset of observations composing the training set
prop_test = round(0.10*n_obs) #subset of observations for test set

training_index = sample(n_obs, prop_training) #samples integers from 1 to n_obs

test_index = dataset$X[-training_index]


train_set <- dataset %>%
  subset(X %in% training_index)

test_set <- dataset %>%
  subset(!(X %in% training_index))

# Correlation Matrix ------------------------------------------------------------------------------------

corr_variables <- cor(dataset) #matrix of correlation of all variables
corr_matrix <- corrplot(corr_variables)
return(corr_matrix)
head(corr_matrix[14,])

# Best Subset Selection ---------------------------------------------------------------------------------









