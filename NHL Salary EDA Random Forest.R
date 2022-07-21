# PURPOSE: Use Random Forest to help in dimension reduction


# Get data set up ---------------------------------------------------------

library(tidyverse)
salaryAllSeasons <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2019-22.csv")

library(stats)
# Change to dummy data set
salaryAllSeasonsSubset <- salaryAllSeasons %>%
  select(-c(player,
            season)) %>%
  filter(games_played >= 20,
         type != "Entry_Level")
# Change to factor data from character
salaryAllSeasonsSubset[sapply(salaryAllSeasonsSubset, is.character)] <- lapply(salaryAllSeasonsSubset[sapply(salaryAllSeasonsSubset, is.character)], as.factor)
# Change to numeric data
salaryAllSeasonsSubset[sapply(salaryAllSeasonsSubset, is.factor)] <- lapply(salaryAllSeasonsSubset[sapply(salaryAllSeasonsSubset, is.factor)], as.numeric)


# Do random forest regression ---------------------------------------------

# Make the random forest model
set.seed(9)
library(ranger)
randomNHLRegression <- ranger(cap_hit ~ .,
                              salaryAllSeasonsSubset,
                              importance = "impurity",
                              mtry = ncol(salaryAllSeasonsSubset) / 3)
randomNHLRegression

# Find the variable importance
library(vip)
vip(randomNHLRegression,
    geom = "col",
    num_features = 76L) +
  theme_bw()

# Table of the most important variables
vi_scores <- vi(randomNHLRegression,
                sort = T,
                decreasing = T,
                rank = T) %>%
  top_n(-50)


# Do the variance importance by forward -----------------------------------
salaryAllSeasonsSubsetForward <- salaryAllSeasonsSubset %>%
  filter(position == 2) %>%
  select(-position)

set.seed(9)
randomNHLRegressionForward <- ranger(cap_hit ~ .,
                              salaryAllSeasonsSubsetForward,
                              importance = "impurity",
                              mtry = ncol(salaryAllSeasonsSubsetForward) / 3)
randomNHLRegressionForward

vip(randomNHLRegressionForward,
    geom = "col",
    num_features = 76L) +
  theme_bw()

vi_scoresForward <- vi(randomNHLRegressionForward,
                sort = T,
                decreasing = T,
                rank = T) %>%
  top_n(-50)


# Do the variance importance by defense -----------------------------------
salaryAllSeasonsSubsetDefense <- salaryAllSeasonsSubset %>%
  filter(position == 1) %>%
  select(-position)

set.seed(9)
randomNHLRegressionDefense <- ranger(cap_hit ~ .,
                                     salaryAllSeasonsSubsetDefense,
                                     importance = "impurity",
                                     mtry = ncol(salaryAllSeasonsSubsetDefense) / 3)
randomNHLRegressionDefense

vip(randomNHLRegressionDefense,
    geom = "col",
    num_features = 76L) +
  theme_bw()

vi_scoresDefense <- vi(randomNHLRegressionDefense,
                       sort = T,
                       decreasing = T,
                       rank = T) %>%
  top_n(-50)

# See what variables show up the most for whole dataset and position data
importantLassoNames <- readRDS("RawData/lasso_important_names.rds")
importantLmNames <- readRDS("RawData/lm_important_names.rds")
importantLassoNames <- as.data.frame(importantLassoNames) %>%
  rename(Variable = importantLassoNames)

# All data
knownNames <- rbind(as.tibble(importantLassoNames$Variable),
                    as.tibble(vi_scores$Variable),
                    as.tibble(importantLmNames$Variable))

mostImportantVariables <- knownNames %>%
  count(value) %>%
  arrange(desc(n))
mostImportantVariables

# Forward Data
importantLassoNamesForward <- readRDS("RawData/lasso_important_names_forward.rds")
importantLmNamesForward <- readRDS("RawData/lm_important_names_forward.rds")
importantLassoNamesForward <- as.data.frame(importantLassoNamesForward) %>%
  rename(Variable = importantLassoNamesForward)
knownNamesForward <- rbind(as.tibble(importantLassoNamesForward$Variable),
                    as.tibble(vi_scoresForward$Variable),
                    as.tibble(importantLmNamesForward$Variable))
mostImportantVariablesForward <- knownNamesForward %>%
  count(value) %>%
  arrange(desc(n))
mostImportantVariablesForward

# Defense Data
importantLassoNamesDefense <- readRDS("RawData/lasso_important_names_defense.rds")
importantLmNamesDefense <- readRDS("RawData/lm_important_names_defense.rds")
knownNamesDefense <- rbind(as.tibble(importantLassoNamesForward$Variable),
                           as.tibble(vi_scoresDefense$Variable),
                           as.tibble(importantLmNamesDefense$Variable))
mostImportantVariablesDefense <- knownNamesDefense %>%
  count(value) %>%
  arrange(desc(n))
mostImportantVariablesDefense


# Tune the random forests
library(caret)
rf_tune_grid <- 
  expand.grid(mtry = seq(3, 75, by = 3), 
              splitrule = "variance",
              min.node.size = 5)
set.seed(9)
caret_nhl_rf <- 
  train(cap_hit ~ ., 
        data = salaryAllSeasonsSubset,
        method = "ranger", 
        num.trees = 75,
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

xgb_tune <- train(x = as.matrix(select(salaryAllSeasonsSubset, -cap_hit)),
                  y = salaryAllSeasonsSubset$cap_hit,
                  trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, 
                  objective = "reg:squarederror",
                  method = "xgbTree",
                  verbose = TRUE)

xgb_fit_final <- xgboost(data = as.matrix(select(salaryAllSeasonsSubset, -cap_hit)),
                         label = salaryAllSeasonsSubset$cap_hit, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(select(xgb_tune$bestTune,
                                                 -nrounds)), 
                         verbose = 0)

vip(xgb_fit_final) + 
  theme_bw()
