library(tidyverse)
library(stringi)
library(data.table)
library(lubridate)
library(caTools)
library(caret)
library(corrplot)
library(radiant)


fbData <- data.table::fread("FootballData_AllSeasons.csv", sep = ",", header = T)

# format Date and order dataframe chronologically

fbData$Date <- lubridate::dmy(fbData$Date)

fbData <- fbData %>% 
  drop_na(Div) %>% 
  arrange(Date) %>% 
  filter(Season != 1920)

colnames(fbData)


# check missing values for all columns

colSums(is.na(fbData))

# Remove variables with greater than 20% missing values

highNA <- names(fbData[colSums(is.na(fbData))> (nrow(fbData)/5)])

highNA

fbData <- fbData %>% 
  select(-all_of(highNA))

# Check variable types

str(fbData)

# Set character vars to upper case

fbData <- fbData %>% 
  mutate_if(is.character, toupper) 

# Create Match Stub variable and match counts

fbData <- fbData %>% mutate(MatchStub = paste(HomeTeam,AwayTeam, sep = "v")) %>% 
  mutate(MatchStub = str_replace_all(MatchStub, " ", "_")) 

fbData$MatchStub

# Add H2H Count and H2H match sequence index (IDX, chronological)

fbData <- as.data.table(fbData)

fbData[ , `:=`(COUNT = .N, IDX = seq_len(.N)) , by = MatchStub]

head(fbData)

# Set ordered factor levels 

fbData$Div <- factor(fbData$Div, levels = c("E3", "E2", "E1", "E0"), ordered = T)

fbData$Season <- paste0("Sn", fbData$Season)

fbData$Season <- factor(fbData$Season, ordered = T)

head(fbData$Season)

str(fbData$Season)

colnames(fbData) <- str_replace_all(colnames(fbData), "<", "_lt_")
colnames(fbData) <- str_replace_all(colnames(fbData), ">", "_gt_")

# Build Home and Away dataframes

# Build Home Games dataframe

homeGms <- fbData %>% group_by(Season, Div, HomeTeam) %>% 
  mutate(HomeTeamPts = if_else(FTHG > FTAG, 3,
                               if_else(FTHG < FTAG, 0, 1))) %>% 
  summarise(HomeMatches = n(),
            TotalHomePts = sum(HomeTeamPts),
            TotalHomeGoals = sum(FTHG),
            TotalHGA = sum(FTAG)) %>% 
  mutate(HomePtsRank = min_rank(desc(TotalHomePts)),
         HomeGoalsRank = min_rank(desc(TotalHomeGoals)),
         TotalHGA_Rank = min_rank(desc(TotalHGA))) %>% 
  arrange(Season, Div, desc(TotalHomePts))


# write home games dataframe to csv

write_csv(homeGms, "HomeGames.csv")

# Build Away Games dataframe

awayGms <- fbData %>% group_by(Season, Div, AwayTeam) %>% 
  mutate(AwayTeamPts = if_else(FTAG > FTHG, 3,
                               if_else(FTHG < FTAG, 0, 1))) %>% 
  summarise(AwayMatches = n(),
            TotalAwayPts = sum(AwayTeamPts),
            TotalAwayGoals = sum(FTAG),
            TotalAGA = sum(FTHG)) %>% 
  mutate(AwayPtsRank = min_rank(desc(TotalAwayPts)),
         AwayGoalsRank = min_rank(desc(TotalAwayGoals)),
         TotalAGA_Rank = min_rank(desc(TotalAGA))) %>%
  arrange(Season, Div, desc(TotalAwayPts))



# write away games dataframe to csv

write_csv(awayGms, "AwayGames.csv")

# Build All Games dataframe

allGms <- homeGms %>% 
  left_join(awayGms, by = c("Season", "Div", "HomeTeam" = "AwayTeam")) %>% 
  rename(Team = HomeTeam)

allGms

finalTables <- allGms %>% 
  mutate(Played = HomeMatches + AwayMatches,
         GoalsFor = TotalHomeGoals + TotalAwayGoals,
         GoalsAgainst = TotalHGA + TotalAGA,
         GoalDiff = GoalsFor - GoalsAgainst,
         TotalPts = TotalHomePts + TotalAwayPts,
         GoalsFor_Rank = min_rank(desc(GoalsFor)),
         GoalsAgainst_Rank = min_rank(desc(GoalsAgainst)),
         FinalPosition = min_rank(desc(TotalPts))) %>% 
  arrange(Season, Div, desc(TotalPts), desc(GoalDiff))

view(tail(finalTables))


ft <- finalTables %>% group_by(Team) %>% 
  mutate(Last_Season_Div = lag(Div, 1)) %>% 
  mutate(Div_Change = factor(if_else(Last_Season_Div == Div, "None",
                                     if_else(Last_Season_Div > Div, "Down", "Up")), levels = c("Up", "None", "Down"), ordered = T)) %>% 
  mutate(Last_Season_FP = lag(FinalPosition, 1)) %>% 
  arrange(Season, Div, desc(TotalPts), desc(GoalDiff))

ft$Div_Change[is.na(ft$Div_Change)] <- "Up"

ft$Div_Change[ft$Season == "Sn1011"] <- NA

tail(ft)

# Check Season Games totals

table(ft$Played, ft$Season)

# write final tables to csv

write_csv(ft, "FinalTables.csv")

##########################################

temp <- fbData %>% left_join(ft, by = c("HomeTeam" = "Team", "Season", "Div")) %>% 
  distinct(Date, HomeTeam, .keep_all = T) %>% 
  rename(HLS_Div = Last_Season_Div,
         HLS_Div_Change = Div_Change,
         HLS_Final_Pos = Last_Season_FP) 

temp <- temp %>% left_join(ft, by = c("AwayTeam" = "Team", "Season", "Div")) %>% 
  distinct(Date, AwayTeam, .keep_all = T) %>% 
  select(!(ends_with(".y") | ends_with(".x"))) %>% 
  rename(ALS_Div = Last_Season_Div,
         ALS_Div_Change = Div_Change,
         ALS_Final_Pos = Last_Season_FP) 

fssn <- min(temp$Season)

temp <- temp %>% filter(Season != fssn)

fbData <- temp

colnames(fbData)

fbData <- fbData %>% 
  mutate(TotalGoals = FTHG + FTAG,
         NetHomeGoalsAdv = FTHG - FTAG) %>% 
  select(Season, Date, Div, HomeTeam, AwayTeam, FTHG, FTAG, TotalGoals, NetHomeGoalsAdv, FTR, HTHG, HTAG, HTR, 
         HLS_Div, HLS_Div_Change, HLS_Final_Pos, ALS_Div, ALS_Div_Change, ALS_Final_Pos, MatchStub, COUNT, IDX, everything()) %>% 
  arrange(Date)


fbData$GAbove0 <- if_else(fbData$TotalGoals > 0.5, 1, 0)
fbData$GAbove1 <- if_else(fbData$TotalGoals > 1.5, 1, 0)
fbData$GAbove2 <- if_else(fbData$TotalGoals > 2.5, 1, 0)
fbData$GAbove3 <- if_else(fbData$TotalGoals > 3.5, 1, 0)
fbData$GAbove4 <- if_else(fbData$TotalGoals > 4.5, 1, 0)
fbData$GAbove5 <- if_else(fbData$TotalGoals > 5.5, 1, 0)
fbData$GAbove6 <- if_else(fbData$TotalGoals > 6.5, 1, 0)
fbData$GAbove7 <- if_else(fbData$TotalGoals > 7.5, 1, 0)
fbData$GAbove8 <- if_else(fbData$TotalGoals > 8.5, 1, 0)

head(fbData)

############

# Create variables for Home/Away shots (for & against) last six matches 

fbData <- fbData %>% group_by(HomeTeam) %>% 
  mutate(HS_L1 = lag(HS, 1),
         HS_L2 = lag(HS, 2),
         HS_L3 = lag(HS, 3),
         HS_L4 = lag(HS, 4),
         HS_L5 = lag(HS, 5),
         HS_L6 = lag(HS, 6),
         HSA_L1 = lag(AS, 1),
         HSA_L2 = lag(AS, 2),
         HSA_L3 = lag(AS, 3),
         HSA_L4 = lag(AS, 4),
         HSA_L5 = lag(AS, 5),
         HSA_L6 = lag(AS, 6)) %>% 
  ungroup()

fbData <- fbData %>% group_by(AwayTeam) %>% 
  mutate(AS_L1 = lag(AS, 1),
         AS_L2 = lag(AS, 2),
         AS_L3 = lag(AS, 3),
         AS_L4 = lag(AS, 4),
         AS_L5 = lag(AS, 5),
         AS_L6 = lag(AS, 6),
         ASA_L1 = lag(HS, 1),
         ASA_L2 = lag(HS, 2),
         ASA_L3 = lag(HS, 3),
         ASA_L4 = lag(HS, 4),
         ASA_L5 = lag(HS, 5),
         ASA_L6 = lag(HS, 6)) %>% 
  ungroup()

str(fbData)

# Create Total Goals Band variable, possibly useful for modelling later

fbData <- fbData %>% 
  mutate(TotalGoalsBand = cut(TotalGoals, breaks = c(-1, 2.5, 5.5, 50), 
                              labels = c("lt2.5", "gt2.5 to lt5.5", "gt5.5"), 
                              ordered_result = T))


colnames(fbData)

# Create Stake, Return and P&L Vars for the Above 2.5 Goals market using Best Available Odds 

fbd <- fbData %>% mutate(Stake = 1,
                         BOA2.5_Return = if_else(GAbove2 == 1, BbMx_gt_2.5, 0),
                         BOA2.5_Profit = BOA2.5_Return - Stake,
                         BOU2.5_Return = if_else(GAbove2 == 0, BbMx_lt_2.5, 0),
                         BOU2.5_Profit = BOU2.5_Return - Stake)

# Add Month variable, on the off chance it may have some value

fbd$Month <- lubridate::month(fbd$Date)

# create running averages for Home & Away Goals/Shots, for and against. Add 'lag 1' variables for building predictive models

fbd <- fbd %>% group_by(HomeTeam) %>% 
  mutate(HG_RAvg = caTools::runmean(FTHG, k = 6, alg = "C", endrule = "mean", align = "right"),
         HG_RAvg_L1 = lag(HG_RAvg, 1),
         HGA_RAvg = caTools::runmean(FTAG, k = 6, alg = "C", endrule = "mean", align = "right"),
         HGA_RAvg_L1 = lag(HGA_RAvg, 1),
         HS_RAvg = caTools::runmean(HS, k = 6, alg = "C", endrule = "mean", align = "right"),
         HS_RAvg_L1 = lag(HS_RAvg, 1),
         HSA_RAvg = caTools::runmean(AS, k = 6, alg = "C", endrule = "mean", align = "right"),
         HSA_RAvg_L1 = lag(HSA_RAvg, 1)) %>% 
  ungroup()

fbd <- fbd %>% group_by(AwayTeam) %>% 
  mutate(AG_RAvg = caTools::runmean(FTAG, k = 6, alg = "C", endrule = "mean", align = "right"),
         AG_RAvg_L1 = lag(AG_RAvg, 1),
         AGA_RAvg = caTools::runmean(FTHG, k = 6, alg = "C", endrule = "mean", align = "right"),
         AGA_RAvg_L1 = lag(AGA_RAvg, 1),
         AS_RAvg = caTools::runmean(AS, k = 6, alg = "C", endrule = "mean", align = "right"),
         AS_RAvg_L1 = lag(AS_RAvg, 1),
         ASA_RAvg = caTools::runmean(HS, k = 6, alg = "C", endrule = "mean", align = "right"),
         ASA_RAvg_L1 = lag(ASA_RAvg, 1)) %>% 
  ungroup()


write_csv(fbd, "fbData_1011_1819.csv")
