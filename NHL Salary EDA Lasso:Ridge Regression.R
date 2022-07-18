# PURPOSE: To see if separating by position makes the most sense


# Load in needed things ---------------------------------------------------

library(tidyverse)
salary21 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")


# Subset data to get rid of player's name and cap hit % -------------------

salary21Subset <- salary21 %>%
  select(-c(player,
            cap_hit_percent,
            season))

# Dimension Reduction Filtering for games and contract type ---------------

library(stats)
# Change to dummy data set
salary21Subset <- salary21Subset %>%
  filter(games_played >= 20,
         type != "Entry_Level")
# Change to factor data from character
salary21Subset[sapply(salary21Subset, is.character)] <- lapply(salary21Subset[sapply(salary21Subset, is.character)], as.factor)
# Change to numeric data
salary21Subset[sapply(salary21Subset, is.factor)] <- lapply(salary21Subset[sapply(salary21Subset, is.factor)], as.numeric)


# Check what type of dimension is the best --------------------------------

# Make the needed matrices
model_x <- salary21Subset %>%
  select(-cap_hit) %>%
  as.matrix()
model_y <- salary21Subset$cap_hit

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
salary21Subset <- salary21Subset %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <- 
  map_dfr(unique(salary21Subset$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- salary21Subset %>% filter(test_fold == holdout)
            train_data <- salary21Subset %>% filter(test_fold != holdout)
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -cap_hit))
            train_x <- as.matrix(dplyr::select(train_data, -cap_hit))
            # Train models:
            lm_model <- lm(cap_hit ~ ., data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = 1)
            en_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = .5)
            quarter_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = .25)
            three_quarter_model <- cv.glmnet(train_x, train_data$cap_hit, alpha = .75)
            # Return tibble of holdout results:
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   en_preds = as.numeric(predict(en_model, newx = test_x)),
                   quarter_preds = as.numeric(predict(quarter_model, newx = test_x)),
                   three_quarter_preds = as.numeric(predict(three_quarter_model, newx = test_x)),
                   test_actual = test_data$cap_hit, test_fold = holdout) 
          })

# Compute the RMSE across folds with standard error intervals
holdout_predictions %>%
  pivot_longer(lm_preds:three_quarter_preds, 
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
best_lambda <- lasso_fit$lambda.min
best_model <- glmnet(model_x, model_y, alpha = .25, lambda = best_lambda)
coef(best_model)
names(best_model$beta[, 1][best_model$beta[, 1] != 0])
