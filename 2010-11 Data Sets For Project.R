# PURPOSE: To explore some of the data we have with the NHL player Stats and Their Salary For 2010-11 Season



# Load the required packages ----------------------------------------------

library(RSelenium)
library(rvest)
library(tidyverse)




# Load the Cap Friendly Data ----------------------------------------------


# Input Cap Friendly Data
salaryCF10 <- read_csv("RawData/CapFriendly 2010-11 Data.csv")
# Make a new column to show if player is forward or defense
library(stringr)
# Make functions here
firstposition <- function(playerposition){
  unlist(str_split(playerposition, ","))[1]
}
realposition <- function(playerposition){
  unlist(str_split(playerposition, "/"))[1]
}
#Change columns to what is needed
salaryCF10$position <- sapply(salaryCF10$position, firstposition)
salaryCF10$position <- sapply(salaryCF10$position, realposition)

salaryCF10 <- salaryCF10 %>%
  mutate(position = ifelse(position %in% c("LW", "C", "RW"), "F", position),
         position = ifelse(position %in% c("LD", "RD", "D"), "D", position))


# Load in the player data from MoneyPuck ----------------------------------


salaryMPplayer10 <- read_csv("RawData/skatersMP2010-11.csv")

# Get rid of not needed variables
salaryMPplayer10 <- salaryMPplayer10  %>%
  mutate(assists = I_F_primaryAssists + I_F_secondaryAssists) %>%
  select(-c(playerId,
            season,
            contains("shifts"),
            gameScore,
            iceTimeRank,
            contains("flurry"),
            contains("scoreVenue"),
            I_F_xFreeze,
            I_F_freeze,
            I_F_playStopped,
            I_F_xPlayStopped,
            contains("scoreAdjusted"),
            contains("Fenwick"),
            I_F_savedUnblockedShotAttempts,
            I_F_xGoals_with_earned_rebounds,
            I_F_flyShiftEnds,
            I_F_faceOffsWon,
            I_F_penalityMinutes,
            I_F_primaryAssists,
            I_F_secondaryAssists))

# Make positions into forwards and defense
salaryMPplayer10 <- salaryMPplayer10 %>%
  mutate(position = ifelse(position %in% c("L", "C", "R"), "F", position)) %>%
  rename(player = name) %>%
  select(player, position, everything())

# Make difference variables needed for later

# Do on ice vs off ice stats
nhl_onIceStats  <- salaryMPplayer10 %>%
  select(player,
         position,
         games_played,
         situation,
         contains("onIce")) %>%
  pivot_longer(cols = -c(player,
                         position,
                         games_played,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "onIce_"))

nhl_offIceStats  <- salaryMPplayer10 %>%
  select(player,
         position,
         games_played,
         situation,
         contains("offIce")) %>%
  pivot_longer(cols = -c(player,
                         position,
                         games_played,
                         situation),
               names_to = "nhl_stats",
               values_to = "offIce_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "offIce_"))

nhl_diff_table <- nhl_onIceStats %>%
  inner_join(nhl_offIceStats,
             by = c("player",
                    "position",
                    "games_played",
                    "situation",
                    "nhl_stats")) %>%
  mutate(value_diff = onIce_value - offIce_value)

nhl_diff_table <- nhl_diff_table %>%
  pivot_wider(id_cols = c("player",
                          "position",
                          "games_played"),
              names_from = c(nhl_stats,
                             situation),
              names_glue = "diff_{situation}_{nhl_stats}",
              values_from = value_diff)

# On ice for and against difference
nhl_onIceFStats  <- salaryMPplayer10 %>%
  select(player,
         position,
         situation,
         contains("onIce_F")) %>%
  pivot_longer(cols = -c(player,
                         position,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_F_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "OnIce_F_"))

nhl_onIceAStats  <- salaryMPplayer10 %>%
  select(player,
         position,
         situation,
         contains("onIce_A")) %>%
  pivot_longer(cols = -c(player,
                         position,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_A_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "OnIce_A_"))

nhl_onIce_diff_table <- nhl_onIceFStats %>%
  inner_join(nhl_onIceAStats,
             by = c("player",
                    "position",
                    "situation",
                    "nhl_stats")) %>%
  mutate(value_diff = onIce_F_value - onIce_A_value)

nhl_onIce_diff_table <- nhl_onIce_diff_table %>%
  pivot_wider(id_cols = c("player",
                          "position"),
              names_from = c(nhl_stats,
                             situation),
              names_glue = "diff_{situation}_{nhl_stats}",
              values_from = value_diff)


# Select needed variables
salaryMPplayer10_neededVariables <- salaryMPplayer10 %>%
  mutate(diff_numberOfPenalty = penaltiesDrawn - penalties,
         diff_penaltyMinutes = penalityMinutesDrawn - penalityMinutes) %>%
  select(-c(contains("onIce"),
            contains("offIce"),
            games_played,
            penalityMinutes,
            penalityMinutesDrawn,
            penalties,
            penaltiesDrawn))

# Pivot data wider to get variables needed to merge
salaryMPplayer10_neededVariables <- salaryMPplayer10_neededVariables %>%
  pivot_wider(id_cols = c(player, 
                          position,
                          team),
              names_from = situation,
              names_glue = "{situation}_{.value}",
              values_from = icetime:diff_penaltyMinutes)

# Combine all the tables together into one big readable table
finalData_salaryMPplayer10 <- nhl_diff_table %>%
  inner_join(nhl_onIce_diff_table,
             by = c("player",
                    "position")) %>%
  inner_join(salaryMPplayer10_neededVariables,
             by = c("player",
                    "position")) %>%
  select(-c(contains("other"),
            contains("x"),
            contains("shotAttempts"),
            contains("lowDanger"),
            contains("mediumDanger"),
            contains("rebounds"),
            contains("missedShots"),
            contains("reboundGoals"),
            diff_4on5_shotsOnGoal,
            contains("diff_4on5_high"),
            contains("diff_5on4_high"),
            diff_4on5_shotsOnGoal,
            contains("points"),
            contains("playContinued"),
            contains("savedShotsOnGoal"),
            contains("dZoneGive"),
            contains("ZoneShiftEnd"),
            contains("timeOnBench"),
            contains("numberOfPen")))



# Load in the data from NHLFastR ------------------------------------------


salaryFastRplayer10 <- read_csv("RawData/player_box_2011_FastR.csv")

# Select only needed variables
salaryFastRplayer10 <- salaryFastRplayer10 %>%
  select(c(player_full_name, 
           shoots_catches, 
           position_abbreviation, 
           skater_stats_power_play_goals, 
           skater_stats_power_play_assists,
           skater_stats_short_handed_goals, 
           skater_stats_short_handed_assists, 
           skater_stats_even_time_on_ice, 
           skater_stats_power_play_time_on_ice, 
           skater_stats_short_handed_time_on_ice,
           skater_stats_plus_minus,
           game_id)) %>%
  rename(plusMinus = skater_stats_plus_minus)

# Filter positions needed and change variables to needed names to merge
salaryFastRplayer10 <- salaryFastRplayer10 %>%
  filter(position_abbreviation != "G") %>%
  mutate(position = ifelse(position_abbreviation %in% c("LW", "C", "RW"), "F", position_abbreviation)) %>%
  select(-position_abbreviation) %>%
  rename(player = player_full_name) %>%
  select(player, position, everything())

# Make sure there are no players duplicated in each game
salaryFastRplayer10 <- salaryFastRplayer10[!duplicated(salaryFastRplayer10[c("player", "position", "game_id")]), ]

# Combine all the data for season total data
salaryFastRplayer10 <- salaryFastRplayer10 %>%
  select(-game_id) %>%
  group_by(player,
           position,
           shoots_catches) %>%
  summarise(across(where(is.numeric), sum, na.rm = T),
            .groups = "drop")




# Merge all the data together ---------------------------------------------

finalData_salary10 <- salaryCF10 %>%
  inner_join(salaryFastRplayer10,
             by = c("player",
                    "position")) %>%
  inner_join(finalData_salaryMPplayer10,
             by = c("player",
                    "position"))

finalData_salary10 <- finalData_salary10 %>%
  select(player,
         position,
         team,
         season,
         everything())

# Clean the data needed to make good variable names
library(janitor)
finalData_salary10 <- clean_names(finalData_salary10) %>%
  mutate(covid = "pre-covid") %>%
  rename(plusMinus = plus_minus)


# Write csv and rds files to be used for later projects ---------------------------------

write_csv(finalData_salary10, "UsedDataForProject/NHL player Stats and Salary 2010-11.csv")

write_rds(finalData_salary10, "UsedDataForProject/NHL player Stats and Salary 2010-11.rds")
