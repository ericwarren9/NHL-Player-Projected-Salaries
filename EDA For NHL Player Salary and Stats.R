# PURPOSE: To do some EDA for NHL Player Salary Data


# Read in our csv file we already made ------------------------------------

salary21 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")


# Load tidyverse ----------------------------------------------------------

library(tidyverse)


# Look at number of games most people played ------------------------------

salary21 %>%
  ggplot(aes(x = games_played)) +
  stat_bin(binwidth = 5,
                 geom="text", 
                 aes(label=after_stat(count)), 
                 vjust=0) +
  theme_bw()


# Look at the points per game, cap hit, and games played ------------------


salary21 %>%
  mutate(PPG =  round(all_i_f_points  / games_played, 4)) %>%
  ggplot(aes(x = PPG,
             y = cap_hit,
             color = games_played)) +
  geom_point(alpha = 0.3)  +
  theme_bw()


# Look at dimension reduction ---------------------------------------------

library(stats)
# Change to dummy data set
salary21Updated <- salary21
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -cap_hit))
# See what variables have zero variance
which(apply(model_CAPHIT, 2, var) == 0)
# Only use variables that don't have zero variance
model_CAPHIT <- model_CAPHIT[ , which(apply(model_CAPHIT, 2, var) != 0)]
pca_CAPHIT <- prcomp(model_CAPHIT, center = T, scale. = T)

summary(pca_CAPHIT)

# Make an elbow plot
library(broom)
pca_CAPHIT %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC,
             y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_CAPHIT),
             color = "cornflowerblue",
             linetype = "dashed") +
  theme_bw()

# Look at what vectors have the most impact
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", 
  length = grid::unit(8, "pt")
)
library(ggrepel)
pca_CAPHIT %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", 
              values_from = "value") %>%
  mutate(stat_type = ifelse(str_detect(column, "all"), "all",
                            ifelse(str_detect(column, "5on4"), "5on4",
                                   ifelse(str_detect(column, "4on5"), "4on5", 
                                          ifelse(str_detect(column, "5on5"), "5on5", "other"))))) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column,
                      color = stat_type),
                  size = 3) +
  scale_color_manual(values = c("orange", "purple", "green", "brown", "cornflowerblue")) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)


# Dimension Reduction without all columns ---------------------------------

library(stats)
# Change to dummy data set
salary21Updated <- salary21 %>%
  select(-contains("all"))
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -cap_hit))
# See what variables have zero variance
which(apply(model_CAPHIT, 2, var) == 0)
# Only use variables that don't have zero variance
model_CAPHIT <- model_CAPHIT[ , which(apply(model_CAPHIT, 2, var) != 0)]
pca_CAPHIT <- prcomp(model_CAPHIT, center = T, scale. = T)

summary(pca_CAPHIT)

# Make an elbow plot
library(broom)
pca_CAPHIT %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC,
             y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_CAPHIT),
             color = "cornflowerblue",
             linetype = "dashed") +
  theme_bw()

# Look at what vectors have the most impact
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", 
  length = grid::unit(8, "pt")
)
library(ggrepel)
pca_CAPHIT %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", 
              values_from = "value") %>%
  mutate(stat_type = ifelse(str_detect(column, "5on4"), "5on4",
                                   ifelse(str_detect(column, "4on5"), "4on5", 
                                          ifelse(str_detect(column, "5on5"), "5on5", "other")))) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column,
                      color = stat_type),
                  size = 3) +
  scale_color_manual(values = c("orange", "green", "brown", "cornflowerblue")) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)


# Dimension Reduction looking at only all columns -------------------------

library(stats)
# Change to dummy data set
salary21Updated <- salary21 %>%
  select(contains("all"),
         cap_hit)
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -cap_hit))
# See what variables have zero variance
which(apply(model_CAPHIT, 2, var) == 0)
# Only use variables that don't have zero variance
model_CAPHIT <- model_CAPHIT[ , which(apply(model_CAPHIT, 2, var) != 0)]
pca_CAPHIT <- prcomp(model_CAPHIT, center = T, scale. = T)

summary(pca_CAPHIT)

# Make an elbow plot
library(broom)
pca_CAPHIT %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC,
             y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_CAPHIT),
             color = "cornflowerblue",
             linetype = "dashed") +
  theme_bw()

# Look at what vectors have the most impact
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", 
  length = grid::unit(8, "pt")
)
library(ggrepel)
pca_CAPHIT %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column),
                  size = 3) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)


# Dimension Reduction Filtering for games and contract type ---------------

library(stats)
# Change to dummy data set
salary21Updated <- salary21 %>%
  filter(games_played >= 20,
         type != "Entry_Level")
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -cap_hit))
# See what variables have zero variance
which(apply(model_CAPHIT, 2, var) == 0)
# Only use variables that don't have zero variance
model_CAPHIT <- model_CAPHIT[ , which(apply(model_CAPHIT, 2, var) != 0)]
pca_CAPHIT <- prcomp(model_CAPHIT, center = T, scale. = T)

summary(pca_CAPHIT)

# Make an elbow plot
library(broom)
pca_CAPHIT %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC,
             y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_CAPHIT),
             color = "cornflowerblue",
             linetype = "dashed") +
  theme_bw()

# Look at what vectors have the most impact
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", 
  length = grid::unit(8, "pt")
)
library(ggrepel)
pca_CAPHIT %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", 
              values_from = "value") %>%
  mutate(stat_type = ifelse(str_detect(column, "all"), "all",
                            ifelse(str_detect(column, "5on4"), "5on4",
                                   ifelse(str_detect(column, "4on5"), "4on5", 
                                          ifelse(str_detect(column, "5on5"), "5on5", 
                                                 ifelse(str_detect(column, "other"), "other", "category")))))) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column,
                      color = stat_type),
                  size = 3) +
  scale_color_manual(values = c("orange", "green", "brown", "cornflowerblue", "purple", "grey")) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)



# Look at what variables might have the most weight -----------------------


# Get rid of season and make initial regression plot
salary21NoCharacterData <- salary21 %>%
  select(which(sapply(salary21, class) != 'character'), -cap_hit_percent)
test_lm_nhl <- lm(cap_hit ~ ., salary21NoCharacterData)

# Look at initial regression coefficients and see which 100 have the most weight
library(broom)
tidy(test_lm_nhl) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  top_n(100, estimate) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", 
           color = "green") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = FALSE) +
  coord_flip() + 
  theme_bw()



# Look at the correlations between different variables --------------------


# Make correlation matrix for just the all values
library(ggcorrplot)
salary21NoCharacterDataJustAll <- salary21NoCharacterData %>%
  select(contains("all"))
cor_matrix <- round(cor(salary21NoCharacterDataJustAll), 3)
ggcorrplot(cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# Cluster the similar correlated values
exp_cor_matrix <- cor(salary21NoCharacterDataJustAll)
cor_dist_matrix <- 1 - abs(exp_cor_matrix)
cor_dist_matrix <- as.dist(cor_dist_matrix)
library(ggdendro)
nhl_exp_hc <- hclust(cor_dist_matrix,
                     "complete")
ggdendrogram(nhl_exp_hc,
             rotate = T,
             size = 2)
library(dendextend)
cor_dist_matrix %>%
  hclust() %>%
  as.dendrogram() %>%
  set("branches_k_col", 
      k = 5) %>% 
  set("labels_cex", .5) %>%
  ggplot(horiz = TRUE)

# First make function we need to look at RMSE values
set.seed(9)
salary21NoCharacterData <- salary21NoCharacterData %>%
  mutate(test_fold = sample(rep(1:5,
                                length.out = n())))

get_cv_preds <- function(model_formula, data) {
  # generate holdout predictions for every row based season
  map_dfr(unique(data$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- data %>%
              filter(test_fold == holdout)
            train_data <- data %>%
              filter(test_fold != holdout)
            # Train model:
            reg_model <- lm(as.formula(model_formula), data = train_data)
            # Return tibble of holdout results:
            tibble(test_preds = predict(reg_model, newdata = test_data),
                   test_actual = test_data$cap_hit,
                   test_fold = holdout) 
          })
}

# Look at RMSE of different regression models
all_cv_preds <- get_cv_preds(cap_hit ~ ., salary21NoCharacterData)
salary21NoCharacterDataJustAll <- salary21NoCharacterData %>%
  select(-(contains(c("4on5", "5on4", "5on5", "other"))))
set.seed(9)
salary21NoCharacterDataJustAll <- salary21NoCharacterDataJustAll %>%
  mutate(test_fold = sample(rep(1:5,
                                length.out = n())))
only_contains_all_cv_preds <- get_cv_preds(cap_hit ~ ., salary21NoCharacterDataJustAll)
int_only_cv_preds <- get_cv_preds(cap_hit ~ 1, salary21NoCharacterData)

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(only_contains_all_cv_preds, type = "Only-situation-is-all"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point() + 
  coord_flip() + 
  theme_bw()


# Lasso/Ridge Regression --------------------------------------------------

# Make the needed matrices
model_x <- salary21NoCharacterData %>%
  select(-cap_hit) %>%
  as.matrix()
model_y <- salary21NoCharacterData$cap_hit

# Make the ridge fit plot
library(glmnet)
ridge_fit <- cv.glmnet(model_x, model_y, alpha = 0)
plot(ridge_fit)
lasso_fit <- cv.glmnet(model_x, model_y, alpha = 1)
plot(lasso_fit)

# Do cross validation to figure out which alpha level for ridge/lasso is better
set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(model_x)))
cv_en_25 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 1)
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_ridge$cvm), min(cv_lasso$cvm))) # We can see that an alpha level at 0.25 is better

# Do comparison of models for holdout performance
set.seed(9)
salary21NoCharacterData <- salary21NoCharacterData %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <- 
  map_dfr(unique(salary21NoCharacterData$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- salary21NoCharacterData %>% filter(test_fold == holdout)
            train_data <- salary21NoCharacterData %>% filter(test_fold != holdout)
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
best_model <- glmnet(model_x, model_y, alpha = 1, lambda = best_lambda)
coef(best_model)
names(best_model$beta[, 1][best_model$beta[, 1] != 0])

# Make regression tree ----------------------------------------------------

# Use only variables for individual and all categories
salary21All <- salary21 %>%
  select(contains("all"),
         cap_hit)
 # Make the regression model
library(rpart)
init_salary_all_individual_tree <- rpart(formula = cap_hit ~ .,
                                         data = salary21All,
                                         method = "anova")
# Graph a decision tree
set.seed(9)
library(rpart.plot)
rpart.plot(init_salary_all_individual_tree)
# Run 10-fold cross validation to prune
plotcp(init_salary_all_individual_tree)

# Make full tree just to see what it all looks like
full_salary_all_individual_tree <- rpart(formula = cap_hit ~ .,
                                         data = salary21All,
                                         method = "anova",
                                         control = list(cp = 0, xval = 12))
rpart.plot(full_salary_all_individual_tree)
plotcp(full_salary_all_individual_tree)

# Train the data using caret
library(caret)
caret_nhl_tree <- train(cap_hit ~ .,
                        data = salary21All, 
                        method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 20)
ggplot(caret_nhl_tree) + 
  theme_bw()

