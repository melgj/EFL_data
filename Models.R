library(yardstick)

fbd <- read_csv("fbData_1011_1819.csv", col_names = T)

# Build a simple linear regression to predict Total Goals to get a feel for the predictive variables

colnames(fbd)

tg <- fbd %>% select(HLS_Div_Change, ALS_Div_Change, HLS_Final_Pos, ALS_Final_Pos, HS_L1:ASA_L6, HG_RAvg_L1, HGA_RAvg_L1, HS_RAvg_L1,
                      HSA_RAvg_L1, AG_RAvg_L1, AGA_RAvg_L1, AS_RAvg_L1, ASA_RAvg_L1, TotalGoals) 

tg <- na.omit(tg)

tg %>% select(HS_RAvg_L1, HSA_RAvg_L1, AS_RAvg_L1, ASA_RAvg_L1) %>% tail(30)

colSums(is.na(tg))

summary(tg)
str(tg)

tg$HLS_Div_Change <- factor(tg$HLS_Div_Change, levels = c("Up", "None", "Down"), ordered = T)
tg$ALS_Div_Change <- factor(tg$ALS_Div_Change, levels = c("Up", "None", "Down"), ordered = T)
str(tgmc$HLS_Div_Change)

# check correlations & near zero vars

numVars <- tg %>% select_if(is.numeric) %>% cor()

findCorrelation(numVars, cutoff = 0.7, names = T)

nzv <- nearZeroVar(numVars, names = T)

tgm <- tg %>% 
  select(HLS_Div_Change, HLS_Final_Pos, ALS_Div_Change, ALS_Final_Pos, TotalGoals, ends_with("L1"))

# build a few simple models

id <- createDataPartition(tgm$TotalGoals, p = 0.7, list = F)

tgTrain <- tgm[id,]
tgTest <- tgm[-id,]

ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  verboseIter = T)

set.seed(1234)

plsq <- train(TotalGoals ~ .,
              data = tgTrain,
              method = "pls",
              preProc = c("center", "scale"),
              metric = "RMSE",
              trControl = ctrl)

summary(plsq)
print(plsq)
varImp(plsq)

set.seed(1234)
mars <- train(TotalGoals ~ .,
              data = tgTrain,
              method = "earth",
              preProc = c("center", "scale"),
              metric = "RMSE",
              trControl = ctrl)

summary(mars)

varImp(mars)



xgbGrid <- expand.grid(eta = c(0.01, 0.001),
                       nrounds = c(50, 100),
                       lambda = c(10, 100),
                       alpha = c(0, 1.0))

set.seed(1234)

xgbMod <- train(TotalGoals ~ .,
                data = tgTrain,
                method = "xgbLinear",
                trControl = ctrl, 
                tuneGrid = xgbGrid,  
                metric = "RMSE")

print(xgbMod)

xgbVarImp <- varImp(xgbMod)$importance %>% 
  rownames_to_column() %>%
  rename(Variable = rowname, XGB_Imp = Overall) %>%
  arrange(desc(XGB_Imp))
xgbVarImp

xgbVarPlot <- xgbVarImp %>% 
  filter(XGB_Imp >= 20) %>% 
  ggplot()+
  geom_col(aes(x=reorder(Variable,XGB_Imp,sum), y=XGB_Imp, fill = XGB_Imp))+
  xlab("Variable")+
  ylab("Variable Importance Score")+
  geom_label(aes(Variable, XGB_Imp,label = round(XGB_Imp,2)), nudge_y = -2.5)+
  coord_flip()+
  labs(title = paste("Variable Importance - Total Goals - XGB Linear"))
xgbVarPlot


#########################################################################################################   

# Explore returns/P&L in 2.5 goal markets for various parameter values

colnames(fbd)

fbd %>% drop_na(Stake, GAbove2, BbMx_gt_2.5, BbMx_lt_2.5, TotalGoals) %>% 
  filter(HLS_Div_Change != "None" | ALS_Div_Change != "None") %>% 
  group_by(HLS_Div_Change, ALS_Div_Change) %>%
  summarise(Matches = n(),
            Total_Above_2.5 = sum(GAbove2),
            Total_Below_2.5 = sum(Matches - Total_Above_2.5),
            Avg_Goals = mean(TotalGoals, na.rm = T),
            AvgBOA2.5 = mean(BbMx_gt_2.5),
            AvgBOU2.5 = mean(BbMx_lt_2.5),
            Total_Stake = sum(Stake),
            Back_a2.5_Return = sum(BOA2.5_Return, na.rm = T),
            Back_a2.5_Profit = sum(BOA2.5_Profit, na.rm = T),
            Back_a2.5_ROI = round(sum(BOA2.5_Profit)/sum(Stake), 4),
            Back_u2.5_Return = sum(BOU2.5_Return, na.rm = T),
            Back_u2.5_Profit = sum(BOU2.5_Profit, na.rm = T),
            Back_u2.5_ROI = round(sum(BOU2.5_Profit)/sum(Stake), 4)) %>% 
  arrange(desc(Back_a2.5_ROI))



