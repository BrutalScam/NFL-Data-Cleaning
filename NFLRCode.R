
#****

# Packages

#****

# Loading Packages
library(readxl)
library(tidyverse)
library(funModeling)
library(lubridate)
library(mice)


#****

# Data Importation and Data Overview

#****


# Loading Data sets
NFLelo1 <- read_excel("nfl_elo_latest.xlsx", 
                       sheet = "Sheet 1") # From Sheet 1
NFLelo2 <- read_excel("nfl_elo_latest.xlsx", 
                       sheet = "Sheet1") # From Sheet 2

# Checking Data Structure
df_status(NFLelo1)
df_status(NFLelo2)


#****

# Date Variable Cleaning

#****

# Correcting Date in row 99 of Data 1 to same date as other games on the weekend. 
NFLelo1$date[99] <- NFLelo1$date[100]
# Correcting Date formats for both data set
NFLelo1$date <- as.Date(NFLelo1$date, origin = "1899-12-30")
NFLelo2$date <- as.Date(NFLelo2$date, origin = "1899-12-30")


#****

# Initial Data Cleaning on Logic

#****

# Setting all seasons to be 2020
NFLelo1$season <- 2020
NFLelo2$season <- 2020

# Removing Neutral variable since its has too zeros
NFLelo1 <- NFLelo1 %>% select(-neutral)
NFLelo2 <- NFLelo2 %>% select(-neutral)


#****

# Categorical Variables Cleaning

#****

# Replacing NA in playoff variable with r for regular season matches
NFLelo1$playoff[which(is.na(NFLelo1$playoff))] <- "r"
NFLelo2$playoff[which(is.na(NFLelo2$playoff))] <- "r"

# Defining not in function
'%!in%' <- function(x,y)!('%in%'(x,y))
# Data 1
# Checking the names for team 2 not in team 1
NFLelo1$team2[which((NFLelo1$team2 %!in% NFLelo1$team1) == "TRUE")]
# Replacing Houston with HOU
NFLelo1$team2[which(NFLelo1$team2 == "Houston")] <- "HOU"
# Checking qb2 names against the NA for teams 2
NFLelo1$qb2[which(is.na(NFLelo1$team2))]
# Checking the teams with "Mitchell Trubisky"
NFLelo1$team2[which(NFLelo1$qb2 == "Mitchell Trubisky")]
# Setting teams 2 for "Mitchell Trubisky" as CHI
NFLelo1$team2[which(NFLelo1$qb2 == "Mitchell Trubisky")] <- "CHI"
# Checking the teams with "Carson Wentz"
NFLelo1$team2[which(NFLelo1$qb2 == "Carson Wentz")]
# Setting teams 2 for "Carson Wentz" as PHI
NFLelo1$team2[which(NFLelo1$qb2 == "Carson Wentz")] <- "PHI"

# Data 2
# Checking the names for team 2 not in team 1
NFLelo2$team2[which((NFLelo2$team2 %!in% NFLelo2$team1) == "TRUE")]
# Replacing OAKLAND with OAK
NFLelo2$team2[which(NFLelo2$team2 == "OAKLAND")] <- "OAK"

# Checking qb1 missing entries
# Data 1
which(is.na(NFLelo1$qb1))
#Checking team for missing entries
NFLelo1$team1[which(is.na(NFLelo1$qb1))]
#Checking qb for TB
NFLelo1$qb1[NFLelo1$team1 == "TB"]
#Setting "Tom Brady" as qb1 for all TB matches 
NFLelo1[NFLelo1$team1 == "TB",]
NFLelo1$qb1[NFLelo1$team1 == "TB"] <- "Tom Brady"
#Checking qb for CAR
NFLelo1$qb1[NFLelo1$team1 == "CAR"]
#Setting "Teddy Bridgewater" for all CAR qb1
NFLelo1[NFLelo1$team1 == "CAR",]
NFLelo1$qb1[NFLelo1$team1 == "CAR"] <- "Teddy Bridgewater"

# Data 2
which(is.na(NFLelo2$qb1))
#Checking team for missing entries
NFLelo2$team1[which(is.na(NFLelo2$qb1))]
#Checking qb for JAX
NFLelo2$qb1[NFLelo2$team1 == "JAX"]
#Setting "Mike Glennon" as qb1 in JAX vs HOU match
NFLelo2[NFLelo2$team1 == "JAX",]
NFLelo2$qb1[which(NFLelo2$team1 == "JAX" & NFLelo2$team2 == "HOU")] <- "Mike Glennon"

#Checking qb for KC
NFLelo2$qb1[NFLelo2$team1 == "KC"]
#Setting "Patrick Mahomes" as qb1 in KC vs BUF match
NFLelo2[NFLelo2$team1 == "KC",]
NFLelo2$qb1[which(NFLelo2$team1 == "KC" & NFLelo2$team2 == "BUF")] <- "Patrick Mahomes"


# Checking qb2 missing entries
# Data 1
which(is.na(NFLelo1$qb2))

# Data 2
which(is.na(NFLelo2$qb2))
#Checking team for missing entries
NFLelo2$team2[which(is.na(NFLelo2$qb2))]
#Checking qb for IND
NFLelo2$qb2[NFLelo2$team2 == "IND"]
#Setting "Philip Rivers" as qb1 in DET vs IND match
NFLelo2[NFLelo2$team2 == "IND",]
NFLelo2$qb2[which(NFLelo2$team1 == "DET" & NFLelo2$team2 == "IND")] <- "Mike Glennon"

# Removing records with NA for both qb1 and qb2
which(is.na(NFLelo1$qb1))
which(is.na(NFLelo1$qb2))
which(is.na(NFLelo2$qb1))
which(is.na(NFLelo2$qb2))

NFLelo2 <- NFLelo2[-163,]

# Data 2
# Checking Team 1 missing entries
which(is.na(NFLelo2$team1))
#Checking qb1 for missing entries
NFLelo2$qb1[which(is.na(NFLelo2$team1))]
#Checking team for "Kyler Murray"
NFLelo2$team1[NFLelo2$qb1 == "Kyler Murray"]
#Setting ARI for all teams with qb1 "Kyler Murray"
NFLelo2[NFLelo2$qb1 == "Kyler Murray",]
NFLelo2$team1[NFLelo2$qb1 == "Kyler Murray"] <- "ARI"
#Checking team for "Aaron Rodgers"
NFLelo2$team1[NFLelo2$qb1 == "Aaron Rodgers"]
#Setting GB for all teams with qb1 Aaron Rodgers"
NFLelo2[NFLelo2$qb1 == "Aaron Rodgers",]
NFLelo2$team1[NFLelo2$qb1 == "Aaron Rodgers"] <- "GB"
#Checking team for "John Wolford"
NFLelo2$team1[NFLelo2$qb1 == "John Wolford"]
NFLelo2$team2[NFLelo2$qb2 == "John Wolford"]
#Setting LAR for all teams with qb1 "John Wolford"
NFLelo2[NFLelo2$qb1 == "John Wolford",]
NFLelo2$team1[NFLelo2$qb1 == "John Wolford"] <- "LAR"

# Checking Team 2 missing entries
which(is.na(NFLelo2$team2))
#Checking qb2 for missing entries
NFLelo2$qb2[which(is.na(NFLelo2$team2))]
#Checking team for "Tua Tagovailoa"
NFLelo2$team2[NFLelo2$qb2 == "Tua Tagovailoa"]
#Setting MIA for all teams with qb2 "Tua Tagovailoa"
NFLelo2[NFLelo2$qb2 == "Tua Tagovailoa",]
NFLelo2$team2[NFLelo2$qb2 == "Tua Tagovailoa"] <- "MIA"


#****

# Numerical Variables Cleaning

#****

# Re-checking data structures
df_status(NFLelo1)
df_status(NFLelo2)

# Setting numeric type for wrong character type variables
NFLelo1$elo1_pre <- as.numeric(NFLelo1$elo1_pre)
NFLelo1$elo2_post <- as.numeric(NFLelo1$elo2_post)
NFLelo1$qb1_value_pre <- as.numeric(NFLelo1$qb1_value_pre)
NFLelo1$qb2_value_pre <- as.numeric(NFLelo1$qb2_value_pre)

NFLelo2$qb1_value_pre <- as.numeric(NFLelo2$qb1_value_pre)
NFLelo2$qb2_value_pre <- as.numeric(NFLelo2$qb2_value_pre)
NFLelo2$qb1_adj <- as.numeric(NFLelo2$qb1_adj)
NFLelo2$score1 <- as.numeric(NFLelo2$score1)
NFLelo2$score2 <- as.numeric(NFLelo2$score2)

# Saving order of variables in data
names <- c(colnames(NFLelo1))

# Re-checking data structures
df_status(NFLelo1)
df_status(NFLelo2)

# Subset selection for numeric variables
NFLelo1.miss <- NFLelo1 %>% select(-c(date, season, playoff, team1, team2, qb1, qb2, score1, score2))
NFLelo1.nmiss <- NFLelo1 %>% select(date, season, playoff, team1, team2, qb1, qb2, score1, score2)

NFLelo2.miss <- NFLelo2 %>% select(-c(date, season, playoff, team1, team2, qb1, qb2))
NFLelo2.nmiss <- NFLelo2 %>% select(date, season, playoff, team1, team2, qb1, qb2)


# Imputing missing entries using pmm method for mean imputation based regression since variables are not exactly independent 
# Imputation
NFLelo1.imputted <- mice(NFLelo1.miss, m = 5, method = "pmm")
NFLelo2.imputted <- mice(NFLelo2.miss, m = 5, method = "pmm")

# Getting Data
NFLelo1.imputted.data <- complete(NFLelo1.imputted, 3)
NFLelo2.imputted.data <- complete(NFLelo2.imputted, 3)


#****

# Combining Data sets

#****

# Combining data set 1
NFLelo1.full <- cbind(NFLelo1.nmiss, NFLelo1.imputted.data)
# Re-checking data structures and replacing remaining missing entries with mean
df_status(NFLelo1.full)
NFLelo1.full$qb1_adj <- as.numeric(NFLelo1.full$qb1_adj)
df_status(NFLelo1.full)
NFLelo1.full$qb1_adj[which(is.na(NFLelo1.full$qb1_adj))] <- mean(NFLelo1.full$qb1_adj, na.rm = T)
# Rounding off and ordering the variables
NFLelo1.full[,8:29] <- round(NFLelo1.full[,8:29], 2)
NFLelo1.full <- NFLelo1.full[, names]


# Combining data set 2
NFLelo2.full <- cbind(NFLelo2.nmiss, NFLelo2.imputted.data)
# Re-checking data structures and replacing remaining missing entries with mean
df_status(NFLelo2.full)
NFLelo2.full$elo_prob2[which(is.na(NFLelo2.full$elo_prob2))] <- mean(NFLelo2.full$elo_prob2, na.rm = T)
NFLelo2.full$qbelo_prob2[which(is.na(NFLelo2.full$qbelo_prob2))] <- mean(NFLelo2.full$qbelo_prob2, na.rm = T)
# Rounding off and ordering the variables
NFLelo2.full[,8:29] <- round(NFLelo2.full[,8:29], 2)
NFLelo2.full <- NFLelo2.full[, names]

# Combining Data sets 1 and 2
NFLelo.combined <- rbind(NFLelo1.full, NFLelo2.full)

# Saving Data set 
write.csv(NFLelo.combined, file = "NFLelo.combined.csv", row.names = F)


#****

# Visualizing Data 

#****

# Visualizations 1: elo for teams across the season
#Sub-setting for date and the elo for the home and away teams
NFLelo.combined.eloPre <- subset(NFLelo.combined, select = c(date, elo1_pre, elo2_pre))
#Re-structuring the Data 
NFLelo.combined.eloPre.gather <- NFLelo.combined.eloPre %>% gather(key = "team", value = "eloPre", 2:3)
#Plot
ggplot(NFLelo.combined.eloPre.gather) +
  aes(x = date, y = eloPre, colour = team, group = team) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(
    title = "elo Pre-match Evolution Throughtout the Season"
  ) +
  theme_minimal()


# Visualizations 2: Scores for teams across the season
#Sub-setting for date and the scores for the home and away teams
NFLelo.combined.scores <- subset(NFLelo.combined, select = c(date, score1, score2))
#Re-structuring the Data 
NFLelo.combined.score.gather <- NFLelo.combined.scores %>% gather(key = "team", value = "score", 2:3)
#Plot
ggplot(NFLelo.combined.score.gather) +
  aes(x = date, y = score, colour = team, group = team) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(title = "Team Score Evolution Throughout the Season") +
  theme_minimal()


