# PURPOSE: Make cubist model 


# Load data and packages --------------------------------------------------

library(tidyverse)
library(Cubist)

salaryAllSeasons <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2019-22.csv")

salaryAllSeasons[sapply(salaryAllSeasons, is.character)] <- lapply(salaryAllSeasons[sapply(salaryAllSeasons, is.character)], as.factor)


# Make cubist model -------------------------------------------------------

# Make training data set
set.seed(9)
in_train_set <- sample(1:nrow(salaryAllSeasons),
                       floor(.8 * nrow(salaryAllSeasons)))

# Make predictors
predictors <- colnames(salaryAllSeasons %>% 
                         select(-c(player,
                                   cap_hit)))

# Separate training and test data
train_pred <- salaryAllSeasons[in_train_set, predictors]
test_pred <- salaryAllSeasons[-in_train_set, predictors]

train_resp <- salaryAllSeasons$cap_hit[in_train_set]
test_resp <- salaryAllSeasons$cap_hit[-in_train_set]

# Make the cubist model
set.seed(9)
model_tree <- cubist(x = train_pred,
                     y = train_resp,
                     committees = 77)

# Get the summary of the model
summary(model_tree)


# Make the model better ---------------------------------------------------


# Do for loop to check how the neighbors change the plot
neighbor <- NULL
rmse <- NULL
r_squared <- NULL

for (i in 1:10) {
  model_tree_pred <- predict(model_tree, 
                             test_pred,
                             neighbors = i-1)
  rmse_value <- sqrt(mean(model_tree_pred - test_resp) ** 2)
  r_square_value <- cor(model_tree_pred, test_resp) ** 2
  neighbor[i] = i-1
  rmse[i] = rmse_value
  r_squared[i] = r_square_value
}

# Look at which neighbor is the best
cubistModelResults <- as_tibble(cbind(neighbor, rmse, r_squared))


# Make needed adjustments -------------------------------------------------

# Make the updated cubist model
set.seed(9)
model_tree_updated <- cubist(x = train_pred,
                     y = train_resp,
                     committees = 77,
                     neighbor = 0)

# Get the summary of the model
summary(model_tree_updated)

# Show predicted values for model
salaryAllSeasons$projected_cap_hit <- predict(model_tree_updated, salaryAllSeasons)

# Make updated data set with player, team, position, season, their actual cap hit, and the predicted cap hit
playerSalaryActualAndPrediction <-
  salaryAllSeasons %>%
  select(player,
         team,
         position,
         season,
         cap_hit,
         projected_cap_hit)

playerSalaryActualAndPrediction <- playerSalaryActualAndPrediction[order(-playerSalaryActualAndPrediction$projected_cap_hit),]

playerSalaryActualAndPrediction %>%
  ggplot(aes(x = cap_hit,
             y = projected_cap_hit)) +
  geom_point(alpha = 0.3,
             color = "cornflowerblue") +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "red") +
  labs(x = "Actual Player Salaries (in USD)",
       y = "Predicted Player Salaries (in USD)",
       title = "How Players Perform in Regards to Their Salary") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


# See if subsetting by season makes sense ---------------------------------

salary21 <- salaryAllSeasons %>%
  filter(season == "2021-22")
salary20 <- salaryAllSeasons %>%
  filter(season == "2020-21")
salary19 <- salaryAllSeasons %>%
  filter(season == "2019-20")


# Do by 2021-22 season ----------------------------------------------------

# Make training data set
set.seed(9)
in_train_set <- sample(1:nrow(salary21),
                       floor(.8 * nrow(salary21)))

# Make predictors
predictors <- colnames(salary21 %>% 
                         select(-c(player,
                                   cap_hit)))

# Separate training and test data
train_pred <- salary21[in_train_set, predictors]
test_pred <- salary21[-in_train_set, predictors]

train_resp <- salary21$cap_hit[in_train_set]
test_resp <- salary21$cap_hit[-in_train_set]

# Make the cubist model
set.seed(9)
model_tree <- cubist(x = train_pred,
                     y = train_resp,
                     committees = 77)

# Get the summary of the model
summary(model_tree)

# Do for loop to check how the neighbors change the plot
neighbor <- NULL
rmse <- NULL
r_squared <- NULL

for (i in 1:10) {
  model_tree_pred <- predict(model_tree, 
                             test_pred,
                             neighbors = i-1)
  rmse_value <- sqrt(mean(model_tree_pred - test_resp) ** 2)
  r_square_value <- cor(model_tree_pred, test_resp) ** 2
  neighbor[i] = i-1
  rmse[i] = rmse_value
  r_squared[i] = r_square_value
}

# Look at which neighbor is the best
cubistModelResults <- as_tibble(cbind(neighbor, rmse, r_squared))

# Make the updated cubist model
set.seed(9)
model_tree_updated <- cubist(x = train_pred,
                             y = train_resp,
                             committees = 77,
                             neighbor = 2)

# Show predicted values for model
salary21$projected_cap_hit <- predict(model_tree_updated, salary21)

# Make updated data set with player, team, position, season, their actual cap hit, and the predicted cap hit
playerSalaryActualAndPrediction21 <-
  salary21 %>%
  select(player,
         team,
         position,
         season,
         cap_hit,
         projected_cap_hit)

playerSalaryActualAndPrediction21 %>%
  ggplot(aes(x = cap_hit,
             y = projected_cap_hit)) +
  geom_point(alpha = 0.3,
             color = "cornflowerblue") +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "red") +
  labs(x = "Actual Player Salaries (in USD)",
       y = "Predicted Player Salaries (in USD)",
       title = "How Players Perform in Regards to Their Salary") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Do by 2020-21 season ----------------------------------------------------

# Make training data set
set.seed(9)
in_train_set <- sample(1:nrow(salary20),
                       floor(.8 * nrow(salary20)))

# Make predictors
predictors <- colnames(salary20 %>% 
                         select(-c(player,
                                   cap_hit)))

# Separate training and test data
train_pred <- salary20[in_train_set, predictors]
test_pred <- salary20[-in_train_set, predictors]

train_resp <- salary20$cap_hit[in_train_set]
test_resp <- salary20$cap_hit[-in_train_set]

# Make the cubist model
set.seed(9)
model_tree <- cubist(x = train_pred,
                     y = train_resp,
                     committees = 77)

# Get the summary of the model
summary(model_tree)

# Do for loop to check how the neighbors change the plot
neighbor <- NULL
rmse <- NULL
r_squared <- NULL

for (i in 1:10) {
  model_tree_pred <- predict(model_tree, 
                             test_pred,
                             neighbors = i-1)
  rmse_value <- sqrt(mean(model_tree_pred - test_resp) ** 2)
  r_square_value <- cor(model_tree_pred, test_resp) ** 2
  neighbor[i] = i-1
  rmse[i] = rmse_value
  r_squared[i] = r_square_value
}

# Look at which neighbor is the best
cubistModelResults <- as_tibble(cbind(neighbor, rmse, r_squared))

# Make the updated cubist model
set.seed(9)
model_tree_updated <- cubist(x = train_pred,
                             y = train_resp,
                             committees = 77,
                             neighbor = 1)

# Show predicted values for model
salary20$projected_cap_hit <- predict(model_tree_updated, salary20)

# Make updated data set with player, team, position, season, their actual cap hit, and the predicted cap hit
playerSalaryActualAndPrediction20 <-
  salary20 %>%
  select(player,
         team,
         position,
         season,
         cap_hit,
         projected_cap_hit)

playerSalaryActualAndPrediction20 %>%
  ggplot(aes(x = cap_hit,
             y = projected_cap_hit)) +
  geom_point(alpha = 0.3,
             color = "cornflowerblue") +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "red") +
  labs(x = "Actual Player Salaries (in USD)",
       y = "Predicted Player Salaries (in USD)",
       title = "How Players Perform in Regards to Their Salary") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Do by 2019-20 season ----------------------------------------------------

# Make training data set
set.seed(9)
in_train_set <- sample(1:nrow(salary19),
                       floor(.8 * nrow(salary19)))

# Make predictors
predictors <- colnames(salary19 %>% 
                         select(-c(player,
                                   cap_hit)))

# Separate training and test data
train_pred <- salary19[in_train_set, predictors]
test_pred <- salary19[-in_train_set, predictors]

train_resp <- salary19$cap_hit[in_train_set]
test_resp <- salary19$cap_hit[-in_train_set]

# Make the cubist model
set.seed(9)
model_tree <- cubist(x = train_pred,
                     y = train_resp,
                     committees = 77)

# Get the summary of the model
summary(model_tree)

# Do for loop to check how the neighbors change the plot
neighbor <- NULL
rmse <- NULL
r_squared <- NULL

for (i in 1:10) {
  model_tree_pred <- predict(model_tree, 
                             test_pred,
                             neighbors = i-1)
  rmse_value <- sqrt(mean(model_tree_pred - test_resp) ** 2)
  r_square_value <- cor(model_tree_pred, test_resp) ** 2
  neighbor[i] = i-1
  rmse[i] = rmse_value
  r_squared[i] = r_square_value
}

# Look at which neighbor is the best
cubistModelResults <- as_tibble(cbind(neighbor, rmse, r_squared))

# Make the updated cubist model
set.seed(9)
model_tree_updated <- cubist(x = train_pred,
                             y = train_resp,
                             committees = 77,
                             neighbor = 3)

# Show predicted values for model
salary19$projected_cap_hit <- predict(model_tree_updated, salary19)

# Make updated data set with player, team, position, season, their actual cap hit, and the predicted cap hit
playerSalaryActualAndPrediction19 <-
  salary19 %>%
  select(player,
         team,
         position,
         season,
         cap_hit,
         projected_cap_hit)

playerSalaryActualAndPrediction19 %>%
  ggplot(aes(x = cap_hit,
             y = projected_cap_hit)) +
  geom_point(alpha = 0.3,
             color = "cornflowerblue") +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "red") +
  labs(x = "Actual Player Salaries (in USD)",
       y = "Predicted Player Salaries (in USD)",
       title = "How Players Perform in Regards to Their Salary") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


# Final model -------------------------------------------------------------

# Combine the seasons back together in one model
salaryAllSeasonsFinalOutcome <- rbind(salary21, salary20, salary19)

# Look at the salary variables to see how it looks
playerSalaryActualAndPredictionFinalOutcome <-
  salaryAllSeasonsFinalOutcome %>%
  select(player,
         team,
         position,
         season,
         cap_hit,
         projected_cap_hit)


playerSalaryActualAndPredictionFinalOutcome <- playerSalaryActualAndPredictionFinalOutcome[order(-playerSalaryActualAndPredictionFinalOutcome$projected_cap_hit),]
