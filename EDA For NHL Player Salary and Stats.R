# PURPOSE: To do some EDA for NHL Player Salary Data


# Read in our csv file we already made ------------------------------------

salary21 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")


# Load libraries ----------------------------------------------------------

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
  mutate(PPG =  round(all_I_F_points  / games_played, 4)) %>%
  ggplot(aes(x = PPG,
             y = `CAP HIT`,
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
model_CAPHIT <- as.matrix(select(salary21Updated, -`CAP HIT`))
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
model_CAPHIT <- as.matrix(select(salary21Updated, -`CAP HIT`))
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
         `CAP HIT`)
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -`CAP HIT`))
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
         TYPE != "Entry_Level")
# Change to factor data from character
salary21Updated[sapply(salary21Updated, is.character)] <- lapply(salary21Updated[sapply(salary21Updated, is.character)], as.factor)
# Change to numeric data
salary21Updated[sapply(salary21Updated, is.factor)] <- lapply(salary21Updated[sapply(salary21Updated, is.factor)], as.numeric)
# Omit na's
salary21Updated <- salary21Updated %>%
  na.omit()
# Make models
model_CAPHIT <- as.matrix(select(salary21Updated, -`CAP HIT`))
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
