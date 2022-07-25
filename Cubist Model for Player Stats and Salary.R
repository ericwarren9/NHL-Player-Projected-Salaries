# PURPOSE: Make cubist model 


# Load data and packages --------------------------------------------------

library(tidyverse)
library(Cubist)
library(purrr)

salaryAllSeasonsReadIn <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2016-22.csv") %>%
  filter(games_played >= 20)

salaryAllSeasons <- salaryAllSeasonsReadIn

salaryAllSeasons[sapply(salaryAllSeasons, is.character)] <- lapply(salaryAllSeasons[sapply(salaryAllSeasons, is.character)], as.factor)


# Make cubist model -------------------------------------------------------

# Make training data set
set.seed(9)
in_train_set <- sample(1:nrow(salaryAllSeasons),
                       floor(.8 * nrow(salaryAllSeasons)))

# Make predictors
predictors <- colnames(salaryAllSeasons %>% 
                         select(-c(player,
                                   cap_hit,
                                   games_played,
                                   total_cap,
                                   percent_cap_hit)))

# Separate training and test data
train_pred <- salaryAllSeasons[in_train_set, predictors]
test_pred <- salaryAllSeasons[-in_train_set, predictors]

train_resp <- salaryAllSeasons$percent_cap_hit[in_train_set]
test_resp <- salaryAllSeasons$percent_cap_hit[-in_train_set]

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
                     neighbor = 6)

# Get the summary of the model
summary(model_tree_updated)

# Show predicted values for model
salaryAllSeasons$projected_percent_cap_hit <- predict(model_tree_updated, salaryAllSeasons)

salaryAllSeasons <- salaryAllSeasons %>%
  mutate(projected_cap_hit = round(projected_percent_cap_hit * total_cap, 2))

# Make updated data set with player, team, position, season, their actual cap hit, and the predicted cap hit
playerSalaryActualAndPrediction <-
  salaryAllSeasons %>%
  select(player,
         team,
         position,
         season,
         games_played,
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

# Write csv to use for R Shiny App
write_csv(playerSalaryActualAndPrediction, "UsedDataForProject/All Seasons Player Salary Projections Short Version.csv")

# Write RDS to use for R Shiny App
write_rds(playerSalaryActualAndPrediction, "UsedDataForProject/All Seasons Player Salary Projections Short Version.rds")


# Write csv to use for R Shiny App
write_csv(salaryAllSeasons, "UsedDataForProject/Final Prediction Model Data.csv")

# Write RDS to use for R Shiny App
write_rds(salaryAllSeasons, "UsedDataForProject/Final Prediction Model Data.rds")
