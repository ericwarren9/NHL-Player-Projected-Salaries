# PURPOSE: To see what the best model is to use


# Load in needed things ---------------------------------------------------

library(tidyverse)
salaryAllSeasons <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2010-22.csv")


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



# Find the best model -----------------------------------------------------


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
  rename(`Cubist Model` = cubist_preds,
         `Random Forest Model` = random_forest_preds,
         `Linear Regression Model` = lm_preds,
         `Intercept Only Model` = intercept_preds,
         `Lasso Model` = lasso_preds,
         `Ridge Model` = ridge_preds) %>%
  pivot_longer(`Linear Regression Model`:`Random Forest Model`, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse =
              sqrt(mean((test_actual - test_preds) ** 2))) %>% 
  ggplot(aes(x = reorder(type, rmse), y = rmse)) + 
  geom_point() + 
  theme_bw() +
  ggtitle("Cubist Model has the Best RMSE and the Model to Use") +
  xlab("Type of Model") +
  ylab("RMSE") +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))