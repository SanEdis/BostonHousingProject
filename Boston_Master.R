### Boston Housing Project ###

## Prepared By : Samba Njie Jr
## Completed : 10/16/2016

setwd("/Users/sambamamba/Documents/Cal Fall 2016/Stat 154/BostonHousingProject")

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

source('Boston_Functions.R')


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
print(corr_matrix)
head(corr_matrix[14,])

# Best Subset Selection ---------------------------------------------------------------------------------

sum(is.na(dataset)) #determines number of NA values. Since 0, nothing to omit.

response <- train_set$MEDV

M_0 <- mean(response) #null model, 0 predictors, and the RSS is simply the mean of the response variables from 1 to 506 observations
print(M_0)

cat("### Splitting training set into predictor and response... ###\n")

p_space <- train_set[,1:13] # data frame of the predictor space (excluding MEDV response)
response <- train_set$MEDV # vector of response variable MEDV

model <- train_set[1:13] # select the first 13 predictors because this provides us with the highest R-squared via our R-squared assessment above
d = ncol(model)

cat("### Find subsets with maximal R-squared values ###\n")

M_models <- power_set(p_space, response) # stores M_1,...,M_p sets of models






