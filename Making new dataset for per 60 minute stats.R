# PURPOSE: To combine data and look at its properties

# Load packages ----------------------------------------------------------

library(tidyverse)
library(purrr)


# Read in our csv file we already made ------------------------------------

salary21 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")
salary20 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2020-21.csv")
salary19 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2019-20.csv")
salary18 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2018-19.csv")
salary17 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2017-18.csv")
salary16 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2016-17.csv")
salary15 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2015-16.csv")
salary14 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2014-15.csv")
salary13 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2013-14.csv")
salary12 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2012-13.csv")
salary11 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2011-12.csv")
salary10 <- read_csv("UsedDataForProject/NHL Player Stats and Salary 2010-11.csv")


# Do 2021-22 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary21$games_played) # Function for per game stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary21$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary21$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary21$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary21Per60Minutes <- salary21 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2020-21 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary20$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary20$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary20$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary20$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary20Per60Minutes <- salary20 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2019-20 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary19$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary19$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary19$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary19$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary19Per60Minutes <- salary19 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2018-19 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary18$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary18$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary18$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary18$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary18Per60Minutes <- salary18 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2017-18 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary17$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary17$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary17$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary17$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary17Per60Minutes <- salary17 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2016-17 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary16$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary16$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary16$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary16$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary16Per60Minutes <- salary16 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2015-16 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary15$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary15$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary15$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary15$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary15Per60Minutes <- salary15 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2014-15 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary14$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary14$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary14$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary14$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary14Per60Minutes <- salary14 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2013-14 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary13$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary13$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary13$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary13$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary13Per60Minutes <- salary13 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2012-13 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary12$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary12$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary12$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary12$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary12Per60Minutes <- salary12 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2011-12 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary11$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary11$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary11$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary11$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary11Per60Minutes <- salary11 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())

# Do 2010-11 Data ---------------------------------------------------------

# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salary10$games_played) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salary10$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salary10$games_played) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salary10$games_played) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salary10Per60Minutes <- salary10 %>%
  mutate_at(c("all_assists",
              "all_faceoffs_lost",
              "all_faceoffs_won",
              "all_i_f_giveaways",
              "all_i_f_goals",
              "all_i_f_high_danger_goals",
              "all_i_f_high_danger_shots",
              "all_i_f_hits",
              "all_i_f_shots_on_goal",
              "all_i_f_takeaways",
              "all_shots_blocked_by_player",
              "diff_all_goals",
              "diff_all_shots_on_goal",
              "diff_all_high_danger_goals",
              "diff_all_high_danger_shots",
              "all_icetime"),
            per60MinStatsAll) %>%
  mutate_at(c("x5on5_assists",
              "x5on5_faceoffs_lost",
              "x5on5_faceoffs_won",
              "x5on5_i_f_giveaways",
              "x5on5_i_f_goals",
              "x5on5_i_f_high_danger_goals",
              "x5on5_i_f_high_danger_shots",
              "x5on5_i_f_hits",
              "x5on5_i_f_shots_on_goal",
              "x5on5_i_f_takeaways",
              "x5on5_shots_blocked_by_player",
              "diff_5on5_goals",
              "diff_5on5_shots_on_goal",
              "diff_5on5_high_danger_goals",
              "diff_5on5_high_danger_shots",
              "x5on5_icetime"),
            per60MinStats5on5) %>%
  mutate_at(c("x5on4_assists",
              "x5on4_faceoffs_lost",
              "x5on4_faceoffs_won",
              "x5on4_i_f_giveaways",
              "x5on4_i_f_goals",
              "x5on4_i_f_high_danger_goals",
              "x5on4_i_f_high_danger_shots",
              "x5on4_i_f_hits",
              "x5on4_i_f_shots_on_goal",
              "x5on4_i_f_takeaways",
              "x5on4_shots_blocked_by_player",
              "diff_5on4_goals",
              "diff_5on4_shots_on_goal",
              "x5on4_icetime"),
            per60MinStats5on4) %>%
  mutate_at(c("x4on5_assists",
              "x4on5_faceoffs_lost",
              "x4on5_faceoffs_won",
              "x4on5_i_f_giveaways",
              "x4on5_i_f_goals",
              "x4on5_i_f_high_danger_goals",
              "x4on5_i_f_high_danger_shots",
              "x4on5_i_f_hits",
              "x4on5_i_f_shots_on_goal",
              "x4on5_i_f_takeaways",
              "x4on5_shots_blocked_by_player",
              "diff_4on5_goals",
              "x4on5_icetime"),
            per60MinStats4on5) %>%
  # mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
  #        std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
  #        std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
  #        std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE))) %>%
  # select(-c(all_icetime,
  #           x5on5_icetime,
  #           x5on4_icetime,
  #           x4on5_icetime)) %>%
  select(player,
         position,
         team,
         age,
         shoots_catches,
         season,
         games_played,
         cap_hit,
         type,
         everything())


# Make one dataset to use for manipulation --------------------------------

# Make new data set that combines previous seasons
salaryAllSeasonsPer60Minutes <- 
  rbind(salary21Per60Minutes,
        salary20Per60Minutes,
        salary19Per60Minutes,
        salary18Per60Minutes,
        salary17Per60Minutes,
        salary16Per60Minutes,
        salary15Per60Minutes,
        salary14Per60Minutes,
        salary13Per60Minutes,
        salary12Per60Minutes,
        salary11Per60Minutes,
        salary10Per60Minutes)

# Change NA's to zero
salaryAllSeasonsPer60Minutes[is.na(salaryAllSeasonsPer60Minutes)] = 0

# Write csv file for per 60 minutes and standardized stats
write_csv(salaryAllSeasonsPer60Minutes, "UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2010-22.csv")

# Write rds file for per 60 minutes and standardized stats
write_rds(salaryAllSeasonsPer60Minutes, "UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2010-22.rds")
