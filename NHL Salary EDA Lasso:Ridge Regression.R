# PURPOSE: To see if separating by position makes the most sense


# Load in needed things ---------------------------------------------------

library(tidyverse)
salaryAllSeasons <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2019-22.csv")


# Dimension Reduction Filtering for games and contract type ---------------

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


# Check what type of dimension is the best --------------------------------

# Make the needed matrices
model_x <- salaryAllSeasonsSubset %>%
  select(-cap_hit) %>%
  as.matrix()
model_y <- salaryAllSeasonsSubset$cap_hit

# Make the ridge fit plot
library(glmnet)
ridge_fit <- cv.glmnet(model_x, model_y, alpha = 0)
plot(ridge_fit)
lasso_fit <- cv.glmnet(model_x, model_y, alpha = 1)
plot(lasso_fit)

# Do cross validation to figure out which alpha level for ridge/lasso is better
set.seed(9)
fold_id <- sample(rep(1:10, length.out = nrow(model_x)))
cv_en_25 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .5)
cv_en_75 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .75)
cv_ridge <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 1)
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_en_75$cvm), min(cv_ridge$cvm), min(cv_lasso$cvm))) # We can see that an alpha level at 1 is better; lasso is the best one

# Do comparison of models for holdout performance
set.seed(9)
salaryAllSeasonsSubset <- salaryAllSeasonsSubset %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
library(purrr)
library(Cubist)
library(ranger)
holdout_predictions <- 
  map_dfr(unique(salaryAllSeasonsSubset$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- salaryAllSeasonsSubset %>% filter(test_fold == holdout)
            train_data <- salaryAllSeasonsSubset %>% filter(test_fold != holdout)
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -cap_hit))
            train_x <- as.matrix(dplyr::select(train_data, -cap_hit))
            # Train models:
            lm_model <- lm(cap_hit ~ ., data = train_data)
            intercept_only_model <- lm(cap_hit ~ 1, data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = 1)
            cubist_model <- cubist(x = train_x,
                                   y = train_data$cap_hit,
                                   committees = 78,
                                   neighbor = 3)
            random_forest <- ranger(cap_hit ~ .,
                                    train_data,
                                    importance = "impurity",
                                    mtry = ncol(train_data) / 3)
            # Return tibble of holdout results:
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   intercept_preds = predict(intercept_only_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   cubist_preds = predict(cubist_model, newdata= test_data),
                   random_forest_preds = as.numeric(predict(random_forest, data = test_data)$predictions),
                   test_actual = test_data$cap_hit, test_fold = holdout) 
          })

# Compute the RMSE across folds with standard error intervals
holdout_predictions %>%
  pivot_longer(lm_preds:random_forest_preds, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse =
              sqrt(mean((test_actual - test_preds) ** 2))) %>% 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red")

# Find the most important variables
library(glmnet)
best_lambda <- lasso_fit$lambda.min
best_model <- glmnet(model_x, model_y, alpha = 1, lambda = best_lambda)
coef(best_model)
importantLassoNames <- names(best_model$beta[, 1][best_model$beta[, 1] != 0])
saveRDS(importantLassoNames, file = "RawData/lasso_important_names.rds")


# Forward Lasso Model -----------------------------------------------------

# Make the needed matrices
salaryAllSeasonsSubsetForward <- salaryAllSeasonsSubset%>%
  filter(position == 2)
model_x <- salaryAllSeasonsSubsetForward %>%
  select(-cap_hit) %>%
  as.matrix()
model_y <- salaryAllSeasonsSubsetForward$cap_hit

# Make the lasso fit plot
library(glmnet)
lasso_fit <- cv.glmnet(model_x, model_y, alpha = 1)
plot(lasso_fit)

# Find the most important variables
library(glmnet)
best_lambda <- lasso_fit$lambda.min
best_model <- glmnet(model_x, model_y, alpha = 1, lambda = best_lambda)
coef(best_model)
importantLassoNames <- names(best_model$beta[, 1][best_model$beta[, 1] != 0])
saveRDS(importantLassoNames, file = "RawData/lasso_important_names_forward.rds")


# Defense Lasso Model -----------------------------------------------------

# Make the needed matrices
salaryAllSeasonsSubsetDefense <- salaryAllSeasonsSubset%>%
  filter(position == 1)
model_x <- salaryAllSeasonsSubsetDefense %>%
  select(-cap_hit) %>%
  as.matrix()
model_y <- salaryAllSeasonsSubsetDefense$cap_hit

# Make the lasso fit plot
library(glmnet)
lasso_fit <- cv.glmnet(model_x, model_y, alpha = 1)
plot(lasso_fit)

# Find the most important variables
library(glmnet)
best_lambda <- lasso_fit$lambda.min
best_model <- glmnet(model_x, model_y, alpha = 1, lambda = best_lambda)
coef(best_model)
importantLassoNames <- names(best_model$beta[, 1][best_model$beta[, 1] != 0])
saveRDS(importantLassoNames, file = "RawData/lasso_important_names_defense.rds")
