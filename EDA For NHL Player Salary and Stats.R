# PURPOSE: To do some EDA for NHL Player Salary Data


# Read in our csv file we already made ------------------------------------

salaryAllSeasons <- read_csv("UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2019-22.csv")


# Load tidyverse ----------------------------------------------------------

library(tidyverse)


# Look at number of games most people played ------------------------------

salaryAllSeasons %>%
  ggplot(aes(x = games_played)) +
  stat_bin(binwidth = 5,
                 geom="text", 
                 aes(label=after_stat(count)), 
                 vjust=0) +
  theme_bw()


# Look at the points per game, cap hit, and games played ------------------


salaryAllSeasons %>%
  ggplot(aes(x = games_played)) + 
  stat_ecdf() +
  theme_bw()


# Look at dimension reduction ---------------------------------------------

library(stats)
# Change to dummy data set
salaryAllSeasonsUpdated <- salaryAllSeasons
# Change to factor data from character
salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.character)] <- lapply(salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.character)], as.factor)
# Change to numeric data
salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.factor)] <- lapply(salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.factor)], as.numeric)
# Omit na's
salaryAllSeasonsUpdated <- salaryAllSeasonsUpdated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salaryAllSeasonsUpdated, -cap_hit))
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
                                          ifelse(str_detect(column, "5on5"), "5on5", "category"))))) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column,
                      color = stat_type),
                  size = 3) +
  scale_color_manual(values = c("orange", "purple", "green", "brown", "cornflowerblue")) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)


# Dimension Reduction Filtering for games and contract type ---------------

library(stats)
# Change to dummy data set
salaryAllSeasonsUpdated <- salaryAllSeasons %>%
  filter(games_played >= 20,
         type != "Entry_Level")
# Change to factor data from character
salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.character)] <- lapply(salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.character)], as.factor)
# Change to numeric data
salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.factor)] <- lapply(salaryAllSeasonsUpdated[sapply(salaryAllSeasonsUpdated, is.factor)], as.numeric)
# Omit na's
salaryAllSeasonsUpdated <- salaryAllSeasonsUpdated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salaryAllSeasonsUpdated, -cap_hit))
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
                                          ifelse(str_detect(column, "5on5"), "5on5", "category"))))) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label = column,
                      color = stat_type),
                  size = 3) +
  scale_color_manual(values = c("orange", "green", "brown", "cornflowerblue", "purple")) +
  theme_bw()

library(factoextra)
fviz_eig(pca_CAPHIT)



# Look at what variables might have the most weight -----------------------


# Get rid of season and make initial regression plot
salaryAllSeasonsNoCharacterData <- salaryAllSeasonsUpdated %>%
  select(which(sapply(salaryAllSeasons, class) != 'character'))
test_lm_nhl <- lm(cap_hit ~ ., salaryAllSeasonsNoCharacterData)

# Look at initial regression coefficients and see which 50 have the most weight
library(broom)
tidy(test_lm_nhl) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", 
           color = "green") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = FALSE) +
  coord_flip() + 
  theme_bw()

# Look at what variables have the highest absolute value
nhl_lm_variable_weight_compare <- abs(test_lm_nhl$coefficients)
nhl_lm_variable_weight_compare <- nhl_lm_variable_weight_compare[order(nhl_lm_variable_weight_compare, decreasing = T)]
nhl_lm_variable_weight_compare_names <- nhl_lm_variable_weight_compare %>%
  names() %>% 
  as.data.frame()
colnames(nhl_lm_variable_weight_compare_names) <- "Variable"
nhl_lm_variable_weight_compare_names <- head(nhl_lm_variable_weight_compare_names, 50)
write_rds(nhl_lm_variable_weight_compare_names, file = "RawData/lm_important_names.rds")

# Now look at forwards
salaryAllSeasonsNoCharacterDataForward <- salaryAllSeasonsUpdated %>%
  filter(position == 2) %>% 
  select(which(sapply(salaryAllSeasons, class) != 'character'))
test_lm_nhl <- lm(cap_hit ~ ., salaryAllSeasonsNoCharacterDataForward)
nhl_lm_variable_weight_compare <- abs(test_lm_nhl$coefficients)
nhl_lm_variable_weight_compare <- nhl_lm_variable_weight_compare[order(nhl_lm_variable_weight_compare, decreasing = T)]
nhl_lm_variable_weight_compare_names <- nhl_lm_variable_weight_compare %>%
  names() %>% 
  as.data.frame()
colnames(nhl_lm_variable_weight_compare_names) <- "Variable"
nhl_lm_variable_weight_compare_names <- head(nhl_lm_variable_weight_compare_names, 50)
write_rds(nhl_lm_variable_weight_compare_names, file = "RawData/lm_important_names_forward.rds")

# Now look at defense
salaryAllSeasonsNoCharacterDataForward <- salaryAllSeasonsUpdated %>%
  filter(position == 1) %>% 
  select(which(sapply(salaryAllSeasons, class) != 'character'))
test_lm_nhl <- lm(cap_hit ~ ., salaryAllSeasonsNoCharacterDataForward)
nhl_lm_variable_weight_compare <- abs(test_lm_nhl$coefficients)
nhl_lm_variable_weight_compare <- nhl_lm_variable_weight_compare[order(nhl_lm_variable_weight_compare, decreasing = T)]
nhl_lm_variable_weight_compare_names <- nhl_lm_variable_weight_compare %>%
  names() %>% 
  as.data.frame()
colnames(nhl_lm_variable_weight_compare_names) <- "Variable"
nhl_lm_variable_weight_compare_names <- head(nhl_lm_variable_weight_compare_names, 50)
write_rds(nhl_lm_variable_weight_compare_names, file = "RawData/lm_important_names_defense.rds")


# Look at the correlations between different variables --------------------


# Make correlation matrix for just the all values
library(ggcorrplot)
cor_matrix <- round(cor(salaryAllSeasonsNoCharacterData), 3)
ggcorrplot(cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# Cluster the similar correlated values
exp_cor_matrix <- cor(salaryAllSeasonsNoCharacterData)
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
salaryAllSeasonsNoCharacterData <- salaryAllSeasonsNoCharacterData %>%
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
all_cv_preds <- get_cv_preds(cap_hit ~ ., salaryAllSeasonsNoCharacterData)

salaryAllSeasonsNoCharacterDataJustAll <-
  salaryAllSeasonsNoCharacterData %>%
  select(-c(contains("5on5"),
            contains("5on4"),
            contains("4on5")))
only_contains_all_cv_preds <- get_cv_preds(cap_hit ~ ., salaryAllSeasonsNoCharacterDataJustAll)

salaryAllSeasonsNoCharacterDataNoAll <-
  salaryAllSeasonsNoCharacterData %>%
  select(-contains("all"))
not_contains_all_cv_preds <- get_cv_preds(cap_hit ~ ., salaryAllSeasonsNoCharacterDataNoAll)

int_only_cv_preds <- get_cv_preds(cap_hit ~ 1, salaryAllSeasonsNoCharacterData)

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(only_contains_all_cv_preds, type = "Just-All-Situations"),
          mutate(not_contains_all_cv_preds, type = "No-All-Situations"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point() + 
  coord_flip() + 
  theme_bw()
