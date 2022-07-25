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

# Make one dataset to use for manipulation --------------------------------

# Make new data set that combines previous seasons
salaryAllSeasons <- rbind(salary21,
                          salary20,
                          salary19,
                          salary18,
                          salary17,
                          salary16)


# Make per 60 minute stats functions
per60MinStatsAll <- function(x, na.rm = FALSE) (x / salaryAllSeasons$all_icetime * 3600) # Function for per 60 minute stats in all situations
per60MinStats5on5 <- function(x, na.rm = FALSE) (x / salaryAllSeasons$x5on5_icetime * 3600) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats4on5 <- function(x, na.rm = FALSE) (x / salaryAllSeasons$x4on5_icetime * 3600) # Function for per 60 minute stats in 5 on 5 situations
per60MinStats5on4 <- function(x, na.rm = FALSE) (x / salaryAllSeasons$x5on4_icetime * 3600) # Function for per 60 minute stats in 5 on 5 situations

# Make per 60 minute stats
salaryAllSeasonsPer60Minutes <- salaryAllSeasons %>%
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
              "diff_all_high_danger_shots"),
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
              "diff_5on5_high_danger_shots"),
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
              "diff_5on4_shots_on_goal"),
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
              "diff_4on5_goals"),
            per60MinStats4on5) %>%
  mutate(std_all_icetime = as.numeric(scale(all_icetime, center = TRUE, scale = TRUE)),
         std_5on5_icetime = as.numeric(scale(x5on5_icetime, center = TRUE, scale = TRUE)),
         std_5on4_icetime = as.numeric(scale(x5on4_icetime, center = TRUE, scale = TRUE)),
         std_4on5_icetime = as.numeric(scale(x4on5_icetime, center = TRUE, scale = TRUE)),
         std_power_play_goals = as.numeric(scale(skater_stats_power_play_goals, center = TRUE, scale = TRUE)),
         std_shorthanded_goals = as.numeric(scale(skater_stats_short_handed_goals, center = TRUE, scale = TRUE)),
         std_power_play_assists = as.numeric(scale(skater_stats_power_play_assists, center = TRUE, scale = TRUE)),
         std_shorthanded_assists = as.numeric(scale(skater_stats_short_handed_assists, center = TRUE, scale = TRUE)),
         std_plus_minus = as.numeric(scale(plusMinus, center = TRUE, scale = TRUE))) %>%
  select(-c(all_icetime,
            x5on5_icetime,
            x5on4_icetime,
            x4on5_icetime,
            diff_4on5_goals,
            diff_5on4_goals,
            diff_5on4_shots_on_goal,
            skater_stats_power_play_assists,
            skater_stats_power_play_goals,
            skater_stats_short_handed_goals,
            skater_stats_short_handed_assists,
            plusMinus)) %>%
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

# Change NA's to zero
salaryAllSeasonsPer60Minutes[is.na(salaryAllSeasonsPer60Minutes)] = 0

# Write csv file for per 60 minutes and standardized stats
write_csv(salaryAllSeasonsPer60Minutes, "UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2016-22.csv")

# Write rds file for per 60 minutes and standardized stats
write_rds(salaryAllSeasonsPer60Minutes, "UsedDataForProject/NHL Player Stats and Salary Per 60 Minutes and Standardized 2016-22.rds")
