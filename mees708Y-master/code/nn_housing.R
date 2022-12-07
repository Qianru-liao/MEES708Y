rm(list = ls())

library(dplyr)
library(keras)
library(tensorflow)

library(mlbench)


# Data ----

data(BostonHousing)
dim(BostonHousing)
head(BostonHousing)
str(BostonHousing)

# Separate into training and testing
index <- sample(nrow(BostonHousing), nrow(BostonHousing) * 0.7, replace = FALSE)
Dtrain <- BostonHousing[index,]
Dtest <- BostonHousing[-index,]

X_train <- Dtrain %>%
    select(-chas) %>%
    select(-medv) %>%
    scale()

y_train <- Dtrain %>%
    select(medv) %>%
    scale()

X_test <- Dtest %>%
    select(-chas) %>%
    select(-medv) %>%
    scale()

y_test <- Dtest %>%
    select(medv) %>%
    scale()

# Model ----
model <- keras_model_sequential()
model %>%
