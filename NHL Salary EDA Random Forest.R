# PURPOSE: Use Random Forest to help in dimension reduction


# Get data set up ---------------------------------------------------------

library(tidyverse)
salary21 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")

salary21Subset <- salary21 %>%
  select(-c(player,
            cap_hit_percent,
            season))

library(stats)
# Change to dummy data set
salary21Subset <- salary21Subset %>%
  filter(games_played >= 20,
         type != "Entry_Level")
# Change to factor data from character
salary21Subset[sapply(salary21Subset, is.character)] <- lapply(salary21Subset[sapply(salary21Subset, is.character)], as.factor)
# Change to numeric data
salary21Subset[sapply(salary21Subset, is.factor)] <- lapply(salary21Subset[sapply(salary21Subset, is.factor)], as.numeric)


# Do random forest regression ---------------------------------------------

# Make the random forest model
set.seed(9)
library(ranger)
randomNHLRegression <- ranger(cap_hit ~ .,
                              salary21Subset,
                              importance = "impurity",
                              mtry = ncol(salary21Subset) / 3)
randomNHLRegression

# Find the variable importance
library(vip)
vip(randomNHLRegression,
    geom = "col",
    num_features = 370L) +
  theme_bw()

# Table of the most important variables
randomNHLRegression$variable.importance

# Tune the random forests
library(caret)
rf_tune_grid <- 
  expand.grid(mtry = seq(3, 150, by = 3), 
              splitrule = "variance",
              min.node.size = 5)
set.seed(9)
caret_nhl_rf <- 
  train(cap_hit ~ ., 
        data = salary21Subset,
        method = "ranger", 
        num.trees = 150,
        trControl = trainControl(method = "cv", number = 5),
        tuneGrid = rf_tune_grid)
ggplot(caret_nhl_rf) + theme_bw()

# Use xgboost to find the best variables
library(xgboost)
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3), 
                                 gamma = 0,
                                 max_depth = c(1, 2, 3, 4), 
                                 colsample_bytree = 1,
                                 min_child_weight = 1,
                                 subsample = 1)

xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

set.seed(9)

xgb_tune <- train(x = as.matrix(select(salary21Subset, -cap_hit)),
                  y = salary21Subset$cap_hit,
                  trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, 
                  objective = "reg:squarederror",
                  method = "xgbTree",
                  verbose = TRUE)

xgb_fit_final <- xgboost(data = as.matrix(select(salary21Subset, -cap_hit)),
                         label = salary21Subset$cap_hit, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(select(xgb_tune$bestTune,
                                                 -nrounds)), 
                         verbose = 0)

vip(xgb_fit_final) + 
  theme_bw()
