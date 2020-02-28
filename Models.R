library(yardstick)

fbd <- read_csv("fbData_1011_1819.csv", col_names = T)

colnames(fbd)

tg <- fbd %>% select(HLS_Div_Change, ALS_Div_Change, HLS_Final_Pos, ALS_Final_Pos, HS_L1:ASA_L6, HG_RAvg_L1, HGA_RAvg_L1, HS_RAvg_L1,
                      HSA_RAvg_L1, AG_RAvg_L1, AGA_RAvg_L1, AS_RAvg_L1, ASA_RAvg_L1, BbAv_gt_2.5, TotalGoals, GAbove2) 

tg <- na.omit(tg)

tg %>% select(HS_RAvg_L1, HSA_RAvg_L1, AS_RAvg_L1, ASA_RAvg_L1) %>% tail(30)

colSums(is.na(tg))

summary(tg)
str(tg)

tg$HLS_Div_Change <- factor(tg$HLS_Div_Change, levels = c("Up", "None", "Down"), ordered = T)
tg$ALS_Div_Change <- factor(tg$ALS_Div_Change, levels = c("Up", "None", "Down"), ordered = T)


# check correlations & near zero vars

numVars <- tg %>% select_if(is.numeric) %>% cor()

findCorrelation(numVars, cutoff = 0.7, names = T)

nzv <- nearZeroVar(numVars, names = T)

tgm <- tg %>% 
  select(HLS_Div_Change, HLS_Final_Pos, ALS_Div_Change, ALS_Final_Pos, ends_with("L1"), BbAv_gt_2.5, TotalGoals)

# build a few simple regression models to predict Total Match Goals

set.seed(1234)

id <- createDataPartition(tgm$TotalGoals, p = 0.7, list = F)

tgTrain <- tgm[id,]
tgTest <- tgm[-id,]

ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  verboseIter = T)

# Elastic Net model

set.seed(1234)

enetGrid <- expand.grid(.lambda = c(0, 0.5, 1),
                        .fraction = seq(0.1, 1, length = 10))

enet <- train(TotalGoals ~ .,
              data = tgTrain,
              method = "enet",
              preProc = c("center", "scale"),
              metric = "RMSE",
              tuneGrid = enetGrid,
              trControl = ctrl)

plot(enet)
print(enet)
  
enet$finalModel$meanx

saveRDS(enet, "enet_model.RDS")

# MARS model

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:20)

set.seed(1234)
mars <- train(TotalGoals ~ .,
              data = tgTrain,
              method = "earth",
              preProc = c("center", "scale"),
              metric = "RMSE",
              tuneGrid = marsGrid,
              trControl = ctrl)

summary(mars)

varImp(mars)

plot(mars)

mars$finalModel

saveRDS(mars, "mars_model.RDS")

# XGB model

xgbGrid <- expand.grid(eta = c(0.01, 0.001, 0.0001),
                       nrounds = c(100, 150),
                       lambda = c(1, 50, 100),
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

saveRDS(xgbMod, "xgb_model.RDS")

# ToDo: Build classification models to predict goals above/under 2.5 goals 

gc <- tg %>% 
  select(ALS_Div_Change, ALS_Final_Pos, ends_with("L1"), BbAv_gt_2.5, GAbove2)

gc$GA2.5 <- if_else(gc$GAbove2 == 1, "Y", "N")

gc$GA2.5 <- factor(gc$GA2.5, levels = c("Y", "N"))

gc <- gc %>% select(-GAbove2)

levels(gc$GA2.5)

colnames(gc)

prop.table(table(gc$GA2.5))

set.seed(1234)

id <- createDataPartition(gc$GA2.5, p = 0.7, list = F)

gcTrain <- gc[id,]
gcTest <- gc[-id,]

str(gc$GA2.5)

colnames(gcTrain)

ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = T)


# Build XGBoost classifier

xgbGrid <- expand.grid(eta = c(0.01, 0.1),
                         nrounds = 200,
                         max_depth = 4:6,
                         min_child_weight = 2:4,
                         colsample_bytree = 0.75,
                         gamma = 1,
                         subsample = 1)

set.seed(1234)

xgbMod <- train(GA2.5 ~ .,
                data = gcTrain,
                method = "xgbTree",
                trControl = ctrl, 
                tuneGrid = xgbGrid,  
                metric = "ROC")

print(xgbMod)

xgbVarImp <- varImp(xgbMod)$importance %>% 
  rownames_to_column() %>%
  rename(Variable = rowname, XGB_Imp = Overall) %>%
  arrange(desc(XGB_Imp))
xgbVarImp

xgbVarPlot <- xgbVarImp %>% 
  filter(XGB_Imp >= 10) %>% 
  ggplot()+
  geom_col(aes(x=reorder(Variable,XGB_Imp,sum), y=XGB_Imp, fill = XGB_Imp))+
  xlab("Variable")+
  ylab("Variable Importance Score")+
  geom_label(aes(Variable, XGB_Imp,label = round(XGB_Imp,2)), nudge_y = -2.5)+
  coord_flip()+
  labs(title = paste("Variable Importance - Goals > 2.5 market - XGBoost"))
xgbVarPlot

saveRDS(xgbMod, "xgbClass_model.RDS")

# Predict probability and generate raw predictions for match > 2.5 goals

PredsRaw <- predict(xgbMod, newdata = gcTest, type = "raw")

PredsRaw

Preds <- predict(xgbMod, newdata = gcTest, type = "prob")

gcCheck <- gcTest %>% 
  mutate(AvgOdds_Prob = round(1/BbAv_gt_2.5, 2),
         AvgOdds_Pred = factor(if_else(AvgOdds_Prob > 0.50, "Y", "N"), levels = c("Y", "N")),
         Pred_Prob = round(Preds$Y, 2),
         Pred_Raw = PredsRaw,
         Actual = as.numeric(if_else(GA2.5 == "Y", 1, 0)))

#########################################################################################################   

# Explore returns/P&L in 2.5 goal markets for various parameter values

gcCheck %>% 
  filter(Pred_Prob > AvgOdds_Prob) %>% 
  summarise(Matches = n(),
            Total_Above_2.5 = sum(Actual),
            Total_Below_2.5 = sum(Matches - Total_Above_2.5),
            WinPct = (round(Total_Above_2.5/Matches, 2)),
            Back_a2.5_Return = round(sum(BbAv_gt_2.5 * Actual, na.rm = T), 2),
            Back_PL = round(Back_a2.5_Return - Matches, 2),
            Back_ROI = round(Back_PL / Matches, 3))





