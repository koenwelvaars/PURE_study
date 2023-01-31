#Create final model with hyperparameter optimization

#Read data
df_train <- read.csv("df_train.csv", na = "NA")
df_test <- read.csv("df_test.csv", na = "NA")

input_x <- as.matrix(select(df_train, -outcome))
input_y <- df_train$outcome

#Search for best hyperparameters with CV grid-search tuning
set.seed(84)
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(84)
train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE,
  allowParallel = TRUE
)

set.seed(84)
xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)

#Optimal amount of nrounds and eta
nrounds <- 1000

set.seed(84)
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(84)
tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds
  verboseIter = FALSE,
  allowParallel = TRUE 
)

set.seed(84)
xgb_tune <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

xgb_tune$bestTune

#Optimal max depth and minimum child weight
set.seed(84)
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
              c(xgb_tune$bestTune$max_depth:4),
              xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1,2,3),
  subsample = 1
)

set.seed(84)
xgb_tune2 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)

#Optimal column and row sampling
set.seed(84)
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)
set.seed(84)
xgb_tune3 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)

#Optimal gamma
set.seed(84)
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

set.seed(84)
xgb_tune4 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

#Optimal learning rate
set.seed(84)
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

set.seed(84)
xgb_tune5 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

#Combine all optimized hyperparameter settings
set.seed(84)
final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
  )

set.seed(84)
xgb_model <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE)

#Test performance of final model
set.seed(84)
pred.beslisboom <- predict(xgb_model, newdata = df_test, type = "raw")
confmat.beslisboom <-confusionMatrix(as.factor(pred.beslisboom), as.factor(df_test$outcome))
confmat.beslisboom

auc.beslisboom <- predict(xgb_model, type = "prob", newdata = df_test)
boompred <- ROCR::prediction(auc.beslisboom[,2], df_test$outcome)
boomauc <- performance(boompred, "auc")
auc.boom <- round(as.numeric(boomauc@y.values),2)
auc.boom

set.seed(84)
pred.xgboost <- predict(xgb_model, newdata = df_test, type = "prob")[,2]
pred.xgboost <- as.numeric(round(pred.xgboost,2))
xgboost.rocobj <- roc(df_test$outcome, pred.xgboost)

ggroc(xgboost.rocobj)

ciobj <- ci.se(xgboost.rocobj, specificities=seq(0, 1, l=1000))
dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                     lower = ciobj[, 1],
                     upper = ciobj[, 3])

 ggroc(xgboost.rocobj,alpha = 0.9, colour = "black", linetype = 1, size = 1.2) +
   theme_classic() +
   geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=1, color = "black") +
   coord_equal() +
   geom_ribbon(data = dat.ci, aes(x = x, ymin = lower, ymax = upper),fill = "#b80f0a", alpha= 0.6) +
   ggtitle(capture.output(xgboost.rocobj$ci)) +
   ggtitle("ROC Curve of tuned XGBoost model") +
   labs(y = "Sensitivity", x = "Specificity") +
   annotate("text", x = .25, y = .25, fontface = 2, size = 5,
           label = paste("Area Under the Curve =", round(xgboost.rocobj$auc,2)))


#Perform subgroup analysis with final model
library(pROC)

#Create subgroups sex, age, and surgery

#Sex
df_train_male <- df_train %>% 
  filter(sex == 0)

df_train_female <- df_train %>% 
  filter(sex == 1)

df_test_male <- df_test %>% 
  filter(sex == 0)

df_test_female <- df_test %>% 
  filter(sex == 1)

#Age groups <18, 18-45, 45-65, 65+

df_train_leeftijd_18_45 <- df_train %>% 
  filter(age >= 18 & age < 45)


df_train_leeftijd_45_65 <- df_train %>% 
  filter(age >=45 & age <65)

df_train_leeftijd_65 <- df_train %>% 
  filter(age >=65)

df_test_leeftijd_18 <- df_test %>% 
  filter(age < 18)

df_test_leeftijd_18_45 <- df_test%>% 
  filter(age >= 18 & age < 45)

df_test_leeftijd_45_65 <- df_test %>% 
  filter(age >=45 & age <65)

df_test_leeftijd_65 <- df_test %>% 
  filter(age >=65)

#Underwent surgery Yes/No

df_train_okja <- df_train %>% 
  filter(surg_laparoscopic == 1 |
         surg_pnl== 1 |
         surg_transurethral_prostate == 1 |
         surg_scrotal == 1 |
         surg_transurethral_bladder == 1)

df_train_oknee <- df_train %>% 
  filter(surg_laparoscopic == 0 &
         surg_pnl== 0 &
         surg_transurethral_prostate == 0 &
         surg_scrotal == 0 &
         surg_transurethral_bladder == 0)

df_test_okja <- df_test %>% 
  filter(surg_laparoscopic == 1 |
         surg_pnl== 1 |
         surg_transurethral_prostate == 1 |
         surg_scrotal == 1 |
         surg_transurethral_bladder == 1)

df_test_oknee <- df_test %>% 
  filter(surg_laparoscopic == 0 &
         surg_pnl== 0 &
         surg_transurethral_prostate == 0 &
         surg_scrotal == 0 &
         surg_transurethral_bladder == 0)

best_model <- function(train, test){
  
  input_x <- as.matrix(select(train, -outcome))
  input_y <- train$outcome

  set.seed(84)
  xgb_model <- caret::train(
    x = input_x,
    y = input_y,
   trControl = train_control,
   tuneGrid = final_grid,
   method = "xgbTree",
   verbose = TRUE)

  set.seed(84)
  preds <- predict(xgb_model, type = "prob", newdata = test)[,2]
  pred.xgboost <- as.numeric(round(preds,2))
  rocobject <- roc(test$outcome, pred.xgboost)
  
  return(rocobject)

}

#roc object of final XGBoost model
roc_finalmodel <- xgboost.rocobj

#SEX
roc_male <- best_model(df_train_male, df_test_male)
roc_female <- best_model(df_train_female, df_test_female)

#Test for signficant statistical difference compared to the final model
roc.test(xgboost.rocobj, roc_male)
roc.test(xgboost.rocobj, roc_female)

#AGE groups
roc_leeftijd_18_45 <- best_model(df_train_leeftijd_18_45, df_test_leeftijd_18_45)
roc_leeftijd_45_65 <- best_model(df_train_leeftijd_45_65, df_test_leeftijd_45_65)
roc_leeftijd_65 <- best_model(df_train_leeftijd_65, df_test_leeftijd_65)

#Test for signficant statistical difference compared to the final model
roc.test(xgboost.rocobj, roc_leeftijd_18_45)
roc.test(xgboost.rocobj, roc_leeftijd_45_65)
roc.test(xgboost.rocobj, roc_leeftijd_65)

#OPERATIE GEHAD JA/NE
roc_operatie_ja <- best_model(df_train_okja, df_test_okja)
roc_operatie_nee <- best_model(df_train_oknee, df_test_oknee)

#Test for signficant statistical difference compared to the final model
(xgboost.rocobj, roc_operatie_ja)
roc.test(xgboost.rocobj, roc_operatie_nee)
