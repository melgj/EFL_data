library(radiant)

# Load Data

fbd <- read_csv("fbData_1011_1819.csv", col_names = T)

##########################################################

# Function to calculate returns for matches where previous meetings meet goals per match thresholds

# thresh = number of matches where Total Match Goals >= 'goals'
# matches = maximum number of consecutive matches in which 'thresh' must be met
# goals = setTotal Match Goals requirement

# This function will print a summary and write results to a file named "h2hgoals.csv" in the current working directory


h2h <- function(df = fbd, thresh = 0, matches = 0, goals = 0) {
  df2 <- data.frame()
  for(matchup in seq_along(unique(df$MatchStub))) {
    x <- filter(df, df$MatchStub == df$MatchStub[matchup])  
    countT <- 0
    countM <- 0
    for(match in seq_along(x$MatchStub)){
      if(x$IDX[match] < x$COUNT[match]){
        countM <- countM + 1
        if(x$TotalGoals[match] >= goals){
          countT <- countT + 1
          if(countT == thresh & countM <= matches){
            df2 <- bind_rows(df2, x[match + 1,])
            countT <- 0
            countM <- 0
          }
        }
      }
    }
  }
  
  if(nrow(df2 > 0)) {
    
    df2 <- df2 %>% select(Date, Div, Season, MatchStub, HomeTeam, AwayTeam, FTHG, FTAG, TotalGoals, COUNT, IDX, GAbove2, BbMx_gt_2.5) %>%
      drop_na(Date) %>% 
      distinct() %>% 
      mutate(Prev_Matches = matches,
             Threshold = thresh,
             Min_Goals = goals,
             Stake = 1,
             Return = if_else(GAbove2 == 1, BbMx_gt_2.5, 0),
             Profit = round(Return - Stake, 2))
    
    print(
      df2 %>% group_by() %>%
        summarise(Matches = n(),
                  Prev_Matches = median(Prev_Matches),
                  Threshold = median(Threshold),
                  Min_Goals = median(Min_Goals),
                  Staked = round(sum(Stake), 2),
                  Won = sum(GAbove2),
                  PctWon = round(Won/Matches, 2),
                  Returns = round(sum(Return), 2),
                  PL = round(sum(Profit), 2),
                  ROI = round(PL/Staked, 2))
    )
    
    print(paste("Total Results For Query:", nrow(df2)))
    
    view(df2)
    
    write_csv(df2, "h2hgoals.csv")
    
  } else {
    print("No results found for this query, try entering different values for thresh, matches or goals")
  }
  
}

#########################################################

# test h2h function

h2h(thresh = 3, matches = 6, goals = 4)

test <- read_csv("h2hgoals.csv", col_names = T)

view(test)

colnames(test)

test %>% group_by() %>%
  summarise(Matches = n(),
            Prev_Matches = median(Prev_Matches),
            Threshold = median(Threshold),
            Min_Goals = median(Min_Goals),
            Staked = round(sum(Stake), 2),
            Won = sum(GAbove2),
            PctWon = round(Won/Matches, 2),
            Returns = round(sum(Return), 2),
            PL = round(sum(Profit), 2),
            ROI = round(PL/Staked, 2))


