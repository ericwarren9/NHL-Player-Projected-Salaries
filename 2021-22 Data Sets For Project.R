# PURPOSE: To explore some of the data we have with the NHL Player Stats and Their Salary For 2021-22 Season



# Load the required packages ----------------------------------------------

library(RSelenium)
library(rvest)
library(tidyverse)




# Load the Cap Friendly Data ----------------------------------------------


# Make For Loop to get Cap Friendly Data
salaryCF21 <- tibble()
tempData <- tibble()
for (i in 1:32) {
  url <- paste0("https://www.capfriendly.com/browse/active/2022?stats-season=2022&display=caphit-percent,skater-individual-advanced-stats,skater-on-ice-advanced-stats,goalie-advanced-stats,type&hide=team,clauses,handed,salary&pg=", i)
  
  tempData <- (read_html(url) %>%
                  html_nodes(xpath = '//*[@id="brwt"]') %>%
                  html_table())[[1]]
  tempData <- tempData %>% mutate_all(as.character)
  
  salaryCF21 <- bind_rows(salaryCF21, tempData)
}

# Get column names for data that need to be numeric but currently are character
numericValues  <- salaryCF21 %>%
  select(-c("PLAYER", "POS", "TOI", "TYPE", "EXPIRY", "CAP HIT", "CAP HIT %")) %>%
  colnames()


salaryCF21[, numericValues] <- lapply(salaryCF21[, numericValues],function(x){as.numeric(gsub(",", "", x))})

# Convert the 'CAP HIT' to a numeric value
salaryCF21$`CAP HIT` <- as.numeric(gsub('[$,]', '', salaryCF21$`CAP HIT`))

# Convert the 'CAP HIT %' to a numeric value
salaryCF21$`CAP HIT %` <- as.numeric(gsub("%", "", salaryCF21$`CAP HIT %`))

# Change all percentages into decimals
percentageValues <- salaryCF21 %>%
  select(c("SF%", "CF%", "FF%", "xGF%", "CAP HIT %")) %>%
  colnames()

salaryCF21[, percentageValues] <- lapply(salaryCF21[, percentageValues],function(x){x / 100})

# Convert the time on ice to seconds of game play
library(lubridate)
salaryCF21$TOI <- substr(salaryCF21$TOI, start = 1, stop = 5)
time21 <- ms(salaryCF21$TOI)
salaryCF21$TOI <- time21@minute * 60 + time21@.Data

# Separate player name and Rank to use just player name
salaryCF21 <- salaryCF21 %>%
  separate(PLAYER, into = c("RANK", "PLAYER"), sep = "[0-9]+. ")  %>%
  select(-RANK)

# Decide what to keep now that other columns don't have
salaryCF21 <- salaryCF21 %>%
  select(c(PLAYER,
           AGE,
           POS,
           `+/-`,
           ixG60,
           iSh60,
           iCF60,
           `SF%`,
           `CF%`,
           `xGF%`,
           TYPE,
           EXPIRY,
           `CAP HIT`,
           `CAP HIT %`)) %>%
  filter(POS != "G")

# Make a new column to show if player is forward or defense
library(stringr)
# Make functions here
firstPosition <- function(playerPosition){
  unlist(str_split(playerPosition, ","))[1]
}
realPosition <- function(playerPosition){
  unlist(str_split(playerPosition, "/"))[1]
}
#Change columns to what is needed
salaryCF21$POS <- sapply(salaryCF21$POS, firstPosition)
salaryCF21$POS <- sapply(salaryCF21$POS, realPosition)
# Make new columns for the position type
salaryCF21 <- salaryCF21 %>%
  mutate(position = ifelse(POS %in% c("LW", "C", "RW"), "F", POS),
         position = ifelse(POS %in% c("LD", "RD", "D"), "D", position)) %>%
  select(-POS) %>%
  select(PLAYER, position, everything()) %>%
  select(- c(contains("x"),
             `CAP HIT %`,
             contains("60"),
             contains("%")))



# Load in the player data from MoneyPuck ----------------------------------


salaryMPPlayer21 <- read_csv("RawData/skatersMP2021-22.csv")

# Get rid of not needed variables
salaryMPPlayer21 <- salaryMPPlayer21  %>%
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
salaryMPPlayer21 <- salaryMPPlayer21 %>%
  mutate(position = ifelse(position %in% c("L", "C", "R"), "F", position)) %>%
  rename(PLAYER = name) %>%
  select(PLAYER, position, everything())

# Make difference variables needed for later

# Do on ice vs off ice stats
nhl_onIceStats  <- salaryMPPlayer21 %>%
  select(PLAYER,
         position,
         games_played,
         situation,
         contains("onIce")) %>%
  pivot_longer(cols = -c(PLAYER,
                         position,
                         games_played,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "onIce_"))

nhl_offIceStats  <- salaryMPPlayer21 %>%
  select(PLAYER,
         position,
         games_played,
         situation,
         contains("offIce")) %>%
  pivot_longer(cols = -c(PLAYER,
                         position,
                         games_played,
                         situation),
               names_to = "nhl_stats",
               values_to = "offIce_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "offIce_"))

nhl_diff_table <- nhl_onIceStats %>%
  inner_join(nhl_offIceStats,
             by = c("PLAYER",
                    "position",
                    "games_played",
                    "situation",
                    "nhl_stats")) %>%
  mutate(value_diff = onIce_value - offIce_value)

nhl_diff_table <- nhl_diff_table %>%
  pivot_wider(id_cols = c("PLAYER",
                          "position",
                          "games_played"),
              names_from = c(nhl_stats,
                             situation),
              names_glue = "diff_{situation}_{nhl_stats}",
              values_from = value_diff)

# On ice for and against difference
nhl_onIceFStats  <- salaryMPPlayer21 %>%
  select(PLAYER,
         position,
         situation,
         contains("onIce_F")) %>%
  pivot_longer(cols = -c(PLAYER,
                         position,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_F_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "OnIce_F_"))

nhl_onIceAStats  <- salaryMPPlayer21 %>%
  select(PLAYER,
         position,
         situation,
         contains("onIce_A")) %>%
  pivot_longer(cols = -c(PLAYER,
                         position,
                         situation),
               names_to = "nhl_stats",
               values_to = "onIce_A_value") %>%
  mutate(nhl_stats = str_remove(nhl_stats, "OnIce_A_"))

nhl_onIce_diff_table <- nhl_onIceFStats %>%
  inner_join(nhl_onIceAStats,
             by = c("PLAYER",
                    "position",
                    "situation",
                    "nhl_stats")) %>%
  mutate(value_diff = onIce_F_value - onIce_A_value)

nhl_onIce_diff_table <- nhl_onIce_diff_table %>%
  pivot_wider(id_cols = c("PLAYER",
                          "position"),
              names_from = c(nhl_stats,
                             situation),
              names_glue = "diff_{situation}_{nhl_stats}",
              values_from = value_diff)


# Select needed variables
salaryMPPlayer21_neededVariables <- salaryMPPlayer21 %>%
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
salaryMPPlayer21_neededVariables <- salaryMPPlayer21_neededVariables %>%
  pivot_wider(id_cols = c(PLAYER, 
                          position,
                          team),
              names_from = situation,
              names_glue = "{situation}_{.value}",
              values_from = icetime:diff_penaltyMinutes)

# Combine all the tables together into one big readable table
finalData_salaryMPPlayer21 <- nhl_diff_table %>%
  inner_join(nhl_onIce_diff_table,
             by = c("PLAYER",
                    "position")) %>%
  inner_join(salaryMPPlayer21_neededVariables,
             by = c("PLAYER",
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


salaryFastRPlayer21 <- read_csv("RawData/player_box_2022_FastR.csv")

# Select only needed variables
salaryFastRPlayer21 <- salaryFastRPlayer21 %>%
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
           game_id))

# Filter positions needed and change variables to needed names to merge
salaryFastRPlayer21 <- salaryFastRPlayer21 %>%
  filter(position_abbreviation != "G") %>%
  mutate(position = ifelse(position_abbreviation %in% c("LW", "C", "RW"), "F", position_abbreviation)) %>%
  select(-position_abbreviation) %>%
  rename(PLAYER = player_full_name) %>%
  select(PLAYER, position, everything())

# Make sure there are no players duplicated in each game
salaryFastRPlayer21 <- salaryFastRPlayer21[!duplicated(salaryFastRPlayer21[c("PLAYER", "position", "game_id")]), ]

# Combine all the data for season total data
salaryFastRPlayer21 <- salaryFastRPlayer21 %>%
  select(-game_id) %>%
  group_by(PLAYER,
           position,
           shoots_catches) %>%
  summarise(across(where(is.numeric), sum, na.rm = T),
            .groups = "drop")
  



# Merge all the data together ---------------------------------------------

finalData_salary21 <- salaryCF21 %>%
  inner_join(salaryFastRPlayer21,
             by = c("PLAYER",
                    "position")) %>%
  inner_join(finalData_salaryMPPlayer21,
             by = c("PLAYER",
                    "position")) %>%
  mutate(season = "2021-22")

finalData_salary21 <- finalData_salary21 %>%
  select(PLAYER,
         position,
         team,
         season,
         everything())

# Clean the data needed to make good variable names
library(janitor)
finalData_salary21 <- clean_names(finalData_salary21) %>%
  rename(plusMinus = x) %>%
  mutate(total_cap = 81500000,
         percent_cap_hit = round(cap_hit / total_cap, 6))


# Write csv and rds files to be used for later projects ---------------------------------

write_csv(finalData_salary21, "UsedDataForProject/NHL Player Stats and Salary 2021-22.csv")

write_rds(finalData_salary21, "UsedDataForProject/NHL Player Stats and Salary 2021-22.rds")
