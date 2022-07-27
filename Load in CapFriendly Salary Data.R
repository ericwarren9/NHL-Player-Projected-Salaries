# PURPOSE: To get some CapFriendly Data from seasons earlier than 2016-17


# Load in the data --------------------------------------------------------

library(tidyverse)

capFriendlyData <- read_rds("RawData/full_capfriendly_contract.rdata") %>%
  filter(season %in% c("2010-11",
                       "2011-12", 
                       "2012-13", 
                       "2013-14", 
                       "2014-15", 
                       "2015-16")) %>%
  filter(position != "G") %>%
  select(name,
         position,
         season,
         birthdate,
         cap_hit,
         contract_number,
         minors_salary) %>%
  rename(player = name) %>%
  as_tibble()


# Make subsets of data to use for later -----------------------------------

library(lubridate)

# 2010-11 Season
capFriendlyData10 <- capFriendlyData %>%
  filter(season == "2010-11") %>%
  mutate(ref_date = mdy(07012010),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 59400000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData10, "RawData/CapFriendly 2010-11 Data.csv")
write_rds(capFriendlyData10, "RawData/CapFriendly 2010-11 Data.rds")

# 2011-12 Season
capFriendlyData11 <- capFriendlyData %>%
  filter(season == "2011-12") %>%
  mutate(ref_date = mdy(07012011),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 64300000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData11, "RawData/CapFriendly 2011-12 Data.csv")
write_rds(capFriendlyData11, "RawData/CapFriendly 2011-12 Data.rds")

# 2012-13 Season
capFriendlyData12 <- capFriendlyData %>%
  filter(season == "2012-13") %>%
  mutate(ref_date = mdy(07012012) - 1,
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 60000000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData12, "RawData/CapFriendly 2012-13 Data.csv")
write_rds(capFriendlyData12, "RawData/CapFriendly 2012-13 Data.rds")

# 2013-14 Season
capFriendlyData13 <- capFriendlyData %>%
  filter(season == "2013-14") %>%
  mutate(ref_date = mdy(07012013) - 1,
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 64300000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData13, "RawData/CapFriendly 2013-14 Data.csv")
write_rds(capFriendlyData13, "RawData/CapFriendly 2013-14 Data.rds")

# 2014-15 Season
capFriendlyData14 <- capFriendlyData %>%
  filter(season == "2014-15") %>%
  mutate(ref_date = mdy(07012014) - 1,
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 69000000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData14, "RawData/CapFriendly 2014-15 Data.csv")
write_rds(capFriendlyData14, "RawData/CapFriendly 2014-15 Data.rds")

# 2015-16 Season
capFriendlyData15 <- capFriendlyData %>%
  filter(season == "2015-16") %>%
  mutate(ref_date = mdy(07012015),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) < 7, year(ref_date) - year(birthdate), 0),
         age = ifelse(as.numeric(substr(birthdate, 6, 7)) >= 7, year(ref_date) - year(birthdate) - 1, age),
         total_cap = 71400000,
         percent_cap_hit = round(cap_hit / total_cap, 6)) %>%
  mutate(type = ifelse(contract_number == 1, "Entry_Level", contract_number),
         type = ifelse(contract_number >= 2 & minors_salary < 100000, "Standard (2-way)", type),
         type = ifelse(contract_number >= 2 & (minors_salary >= 100000 | is.na(minors_salary)), "Standard (1-way)", type)) %>%
  select(-c(ref_date,
            birthdate,
            contract_number,
            minors_salary))
write_csv(capFriendlyData15, "RawData/CapFriendly 2015-16 Data.csv")
write_rds(capFriendlyData15, "RawData/CapFriendly 2015-16 Data.rds")