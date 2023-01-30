#Load libraries, data, functions-file and turn off scientific notation of numbers
packages <- c("ROCR", "mice", "VIM", "ggplot2", "DMwR", "randomForest", "caret", "dplyr", "pscl", "magrittr", "rpart", "rpart.plot", "ipred", "C50", "ggcorrplot", "factoextra", "stringr", "purrr", "tidyr", "reshape2", "RColorBrewer", "glmnet", "xgboost", "gbm", "xtable", "formattable", "data.table", "kableExtra", "sjPlot", "pROC") 
lapply(packages, require, character.only=TRUE)

df_train <- read.csv("df_train.csv")
df_test <- read.csv("df_test.csv")
source("Functions.R")
options(scipen=999)

#Time script - start time
start.script <- Sys.time()

#Create classification models
alg.boom.gin <- beslisboom(df = df_train,
                           x = df_train, 
                           y = "outcome")

plot.beslisboom(alg.boom.gin$finalModel)


#model ROC curve
perf.boom <- roc.boom(uitkomst = df_test$outcome,   
                                     model = alg.boom.gin, 
                                     testdata = df_test)

#model uitkomstmaten
metrics.beslisboom <- uitkomst.model(uitkomst = df_test$outcome,   #default type = prob
                                     model = alg.boom.gin, 
                                     testdata = df_test)
metrics.beslisboom

#Bagged beslisboom
bag.boom <- ml.beslisboom(df = df_train, 
                          x = df_train, 
                          y = "outcome", 
                          method = "treebag")

print(bag.boom)

#model ROC curve
perf.bagboom <- roc.boom(uitkomst = df_test$outcome,   
                                     model = bag.boom, 
                                     testdata = df_test)

#model uitkomstmaten
metrics.bagboom <- uitkomst.model(uitkomst = df_test$outcome, 
                                  model = bag.boom, 
                                  testdata = df_test, 
                                  type = "raw")
metrics.bagboom

#Boosted beslisboom

# Boosted trees
boost.boom <- ml.beslisboom(df = df_train, 
                            x = df_train, 
                            y = "outcome", 
                            method = "gbm")

print(boost.boom)

#model ROC curve
perf.boostboom <- roc.boom(uitkomst = df_test$outcome,   
                                     model = boost.boom, 
                                     testdata = df_test)

#model uitkomstmaten
metrics.boostedboom <- uitkomst.model(uitkomst = df_test$outcome, #default type = prob
                                      model = boost.boom, 
                                      testdata = df_test)
metrics.boostedboom


#Extreme Gradient (XG) Boosted beslisboom
# XG Boosted trees
xgboost.boom <- ml.beslisboom(df = df_train, 
                              x = df_train, 
                              y = "outcome", 
                              method = "xgbTree")


#model ROC curve
perf.xgboostboom <- roc.boom(uitkomst = df_test$outcome,   
                                     model = xgboost.boom, 
                                     testdata = df_test)

#model uitkomstmaten
metrics.xgboostedboom <- uitkomst.model(uitkomst = df_test$outcome, #default type = prob
                                        model = xgboost.boom, 
                                        testdata = df_test)
metrics.xgboostedboom


#Random Forest
rf.boom <- ml.randomforest(df = df_train,
                          x = df_train[,-c("outcome")],
                          y = "unplanned_readmission",
                          ntree = 2000)

print(rf.boom)

#model ROC curve
perf.randomforest <- roc.randomforest(model = rf.boom,
                                      testdata = df_test,
                                      y = df_test$outcome)

#model uitkomstmaten
metrics.randomforest <-  rf.uitkomst.model(model = rf.boom,
                                           testdata = df_test,
                                           y = df_test$outcome)

metrics.randomforest

#Create regression models

#Logistic regrssion (logreg)
log.regressie <- ml.regressie(df = df_train,
                                 x = df_train,
                                 y = "outcome")

#model ROC curve
perf.logreg <- roc.regressie(model = log.regressie,
                                     testdata = df_test,
                                     test_y = df_test$outcome)

#model uitkomstmaten
metrics.logregressie <- uitkomst.regressie(model = log.regressie,
                                           testdata = df_test,
                                           test_y = df_test$outcome)

metrics.logregressie

#LASSO regressie
lasso.regressie <- ml.lassoridge(x_train = df_train, 
                               x_test = df_train,
                               y = "outcome", 
                               y_train = df_train$outcome,
                               y_test = df_test$outcome,
                               alpha = 1) #1 = LASSO // 0 = RIDGE

ridge.regressie <- ml.lassoridge(x_train = df_train, 
                               x_test = df_train,
                               y = "outcome", 
                               y_train = df_train$outcome,
                               y_test = df_test$outcome,
                               alpha = 0) #1 = LASSO // 0 = RIDGE

#model ROC curve
perf.lasso <- roc.lassoridge(x_train = df_train, 
                               x_test = df_train,
                               y = "outcome", 
                               y_train = df_train$outcome,
                               y_test = df_test$outcome,
                             alpha = 1) #1 = LASSO // 0 = RIDGE
  
#model uitkomstmaten
lasso.regressie


#model ROC curve
perf.ridge <- roc.lassoridge(x_train = df_train, 
                               x_test = df_train,
                               y = "outcome", 
                               y_train = df_train$outcome,
                               y_test = df_test$outcome,
                             alpha = 0) #1 = LASSO // 0 = RIDGE

#model uitkomstmaten
ridge.regressie

end.script <- Sys.time()

duration.script <- end.script - start.script
duration.script

#Calibration curves of models

#Load additional libraries and define target
library(tidymodels)
library(probably)

target <- df_test$outcome

preds_boom <- predict(alg.boom.gin, df_test, type = "prob")
model <- "Decision Tree"
cal_boom <- tibble(model,preds_boom,target)

preds_bag <- predict(bag.boom, df_test, type = "prob")
model <- "Bagged Decision Tree"
cal_bag <- tibble(model,preds_bag,target)

preds_boost <- predict(boost.boom, df_test, type = "prob")
model <- "Boosted Decision Tree"
cal_boost <- tibble(model,preds_boost,target)

preds_xgb <- predict(xgboost.boom, df_test, type = "prob")
model <- "XGBoost"
cal_xgb <- tibble(model,preds_xgb,target)

preds_rf <- predict(rf.boom, df_test, type = "prob")
model <- "RandomForest"
cal_rf <- tibble(model,preds_rf,target)

#Format for regression algorithms
Yes <- round(predict(log.regressie, df_test),4)
No <- round(1 - Yes,4)
preds_lr <- tibble(No,Yes)
model <- "Logistic Regression"
cal_lr <- tibble(model,preds_lr,target)

#Format for LASSO and RIDGE regression
x_names <- colnames(select(df_train, -c(outcome)))
x <- paste(x_names, collapse = " + ")
yx_formule.train <- as.formula(paste('outcome', x, sep = " ~ "))
yx_formule.test <- as.formula(paste('outcome', x, sep = " ~ "))

set.seed(84)
lassoridge.train <- model.matrix(yx_formule.train, df_train)
lassoridge.test <- model.matrix(yx_formule.test, df_train)

ridge_reg <- cv.glmnet(lassoridge.train, df_train$outcome, alpha = 0,family = "binomial")
lasso_reg <- cv.glmnet(lassoridge.train, df_train$outcome, alpha = 1,family = "binomial")

Yes <- round(predict(ridge_reg, lassoridge.test, type = "response"),4)
No <- round(1 - Yes,4)
preds_ridge <- tibble(No,Yes)
model <- "Ridge Regression"
cal_ridge <- tibble(model,preds_ridge,target)

Yes <- round(predict(lasso_reg,lassoridge.test, type = "response"),4)
No <- round(1 - Yes,4)
preds_lasso <- tibble(No,Yes)
model <- "Lasso Regression"
cal_lasso <- tibble(model,preds_lasso,target)

#Add all formatted data together in a tibble 
cal_plots <- cal_boom %>% 
  add_row(cal_bag) %>% 
  add_row(cal_boost) %>% 
  add_row(cal_xgb) %>%
  add_row(cal_rf) %>% 
  add_row(cal_lr) %>%
  add_row(cal_ridge) %>% 
  add_row(cal_lasso)

#Plot all the calibration curves
cal_plots %>% 
  group_by(model) %>%
  cal_plot_breaks(target, 
                  No, 
                  include_ribbon = FALSE,
                  include_rug = FALSE) +
  facet_wrap(~model,ncol=4) +
  theme(legend.position = "none") +
  ggtitle("Calibration curves of models") +
  scale_color_manual(values=c("#b80f0a","#b80f0a","#b80f0a","#b80f0a",
                              "#b80f0a","#b80f0a","#b80f0a","#b80f0a")) +
  theme_bw() +
  theme(legend.position="none")

#Additional visualisations of model performance

#Add all information together for sensitivity/specificity plot
type_of_model <- c("Decision Tree", "Bagged Trees", "Boosted Trees", "XG Boosted Trees", "RandomForest", 
                   "Logistic regression", "LASSO regression", "RIDGE regression")

sensitivity <- c(metrics.beslisboom$sens.boom,metrics.bagboom$sens.boom, metrics.boostedboom$sens.boom, 
                 metrics.xgboostedboom$sens.boom, metrics.randomforest$sens.rf, metrics.logregressie$sens.log,
                 lasso.regressie$sens.lassoridge, ridge.regressie$sens.lassoridge)

specificity <- c(metrics.beslisboom$spec.boom,metrics.bagboom$spec.boom, metrics.boostedboom$spec.boom, 
                 metrics.xgboostedboom$spec.boom, metrics.randomforest$spec.rf, metrics.logregressie$spec.log,
                 lasso.regressie$spec.lassoridge, ridge.regressie$spec.lassoridge)

all.confmatrices <- data.frame("Model" = type_of_model, 
                               "Sensitivity" = sensitivity, 
                               "Specificity" = specificity)

all.confmatrices <- all.confmatrices %>%
  as_tibble() %>%
  mutate(Accuracy = sprintf("%0.2f",(sensitivity + specificity)/2)) %>%
  select(Model, Accuracy, Sensitivity, Specificity)

#Add all information together for AUC plot
auc_type_of_model <- c("Decision Tree", "Bagged Trees", "Boosted Trees", "XG Boosted Trees", "RandomForest", 
                   "Logistic regression", "LASSO regression", "RIDGE regression")

auc <- c(metrics.beslisboom$auc.boom, metrics.bagboom$auc.boom, metrics.boostedboom$auc.boom, 
         metrics.xgboostedboom$auc.boom, metrics.randomforest$auc.rf, metrics.logregressie$auc.log, 
         lasso.regressie$auc.lassoridge, ridge.regressie$auc.lassoridge)

all.aucmatrix <- data.frame("Model" = auc_type_of_model, 
                            "Area Under the Curve" = auc)

#Add all information together for precision, recall, and F1 plot
type_of_model <- c("Decision Tree", "Bagged Trees", "Boosted Trees", "XG Boosted Trees", "RandomForest", 
                   "Logistic regression", "LASSO regression", "RIDGE regression")

ppv <- c(metrics.beslisboom$ppv.boom, metrics.bagboom$ppv.boom  , metrics.boostedboom$ppv.boom, 
         metrics.xgboostedboom$ppv.boom, metrics.randomforest$ppv.rf, metrics.logregressie$ppv.log, 
         lasso.regressie$ppv.lassoridge, ridge.regressie$ppv.lassoridge)

npv <- c(metrics.beslisboom$npv.boom, metrics.bagboom$npv.boom, metrics.boostedboom$npv.boom, 
         metrics.xgboostedboom$npv.boom, metrics.randomforest$npv.rf, metrics.logregressie$npv.log, 
         lasso.regressie$npv.lassoridge, ridge.regressie$npv.lassoridge)

all.prf1 <- data.frame("Model" = type_of_model, 
                       "PPV" = ppv, 
                       "NPV" = npv)

#Bind all metrics together
total.matrix <- cbind(all.confmatrices, all.aucmatrix$Area.Under.the.Curve, all.prf1$PPV, all.prf1$NPV)
colnames(total.matrix) <- c("Algorithm", "Accuracy", "Sensitivity", "Specificity", "AUC", "PPV", "NPV")

tabel.total.matrix <- as.data.frame(total.matrix) %>%
  select(Algorithm, Accuracy, Sensitivity, Specificity, AUC, PPV, NPV)

unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
sterke_perf <- formatter("span",
  style = x ~ style(color = ifelse(x >= 0.84, "green",
    ifelse(x <= 0.80, "red", "black"))))

#Plot the outcome matrix
formattable(tabel.total.matrix)
formattable(tabel.total.matrix,
            align = c("l", "c", "c", "c", "c", "c", "c", "c"))

#Save the plot as a Word document
tab_df(tabel.total.matrix,
        file = "table_algorithms_eval.doc")

#Plot sensitivity x specificity
all.confmatrices %>%
  ggplot(aes(x = sensitivity, y = specificity)) +
  geom_point(aes(color = type_of_model), shape = 18, size = 7) +
  xlim(0.6, 1.0) +
  ylim(0.6, 1.0) +
  scale_colour_discrete("Model") +
  geom_abline(color = "grey") +
  labs(title = "Overzicht sens/spec modellen", x = "Sensitiviteit", y = "Specificiteit") +
  theme_classic()


#Plot precision x recall
all.prf1 %>%
  ggplot(aes(x = Recall, y = Precision)) +
  geom_point(aes(color = type_of_model), shape = 18, size = 7) +
  xlim(0.5, 1.0) +
  ylim(0.5, 1.0) +
  scale_colour_discrete("Model") +
  geom_abline(color = "grey") +
  labs(title = "Overzicht prec/rec modellen", x = "Recall", y = "Precision") +
  theme_classic()

#Format data for AUC plot
aucdotplot <-  all.aucmatrix %>%
  arrange(Model) %>% # sort using the numeric variable that interest you
  mutate(x = Model) %>%
  mutate(y = Area.Under.the.Curve)

aucdotplot %>%
  mutate(text = auc_type_of_model) %>%
  ggplot(aes(x=x, y=y)) +
  geom_point(aes(text=auc_type_of_model), size=10, color="seagreen") +
  geom_segment(aes(x=x, xend=x, y=0.65,yend=1.00),color="seagreen") +
  ggtitle("AUC-score per model") +
  xlab('') +
  ylab('') +
  theme_classic() +
  theme(
    legend.position="none",
    axis.line.y = element_blank(),
    axis.text=element_text(angle = 45, hjust = 1, size=10))

#Bind all ROC curves together in a function
roccurvesmodellen <- function(){
  
  plot(perf.boom, col='gold', lwd = 4 , main= "red") +
  plot(perf.bagboom, add=TRUE,lwd = 4, col='forestgreen') +
  plot(perf.boostboom, add=TRUE,lwd = 4, col='orange') +
  plot(perf.xgboostboom,add=TRUE, lwd = 4,col='pink') +
  plot(perf.randomforest,add=TRUE, lwd = 4,  col='darkturquoise') +
  plot(perf.logreg, add=TRUE, lwd = 4, col='darkorchid') +
  plot(perf.lasso,add=TRUE, lwd = 4, col='red1') +
  plot(perf.ridge, add=TRUE, lwd = 4, col='blue2') +
  abline(0, 1, untf = FALSE, col = "black", lty = 4)
  
  # Add a legend
legend("bottomright",
  legend = c("Decision Tree", "Bagged Trees", "Boosted Trees", "XG Boosted Trees", "RandomForest", 
              "Logistic regression", "LASSO regression", "RIDGE regression"),
  col = c("gold", "forestgreen", "orange", "pink", "darkturquoise","darkorchid", "red1", "blue2"),
  pch = c(19),
  bty = "o",
  pt.cex = 4,
  cex = 1,
  text.col = "black",
  horiz = F ,
  inset = c(0, 0))
}

#Execute custom ROC function
roccurvesmodellen()

#Create more customised ROC curve with colors and line types for dinstinction
roclist <- list("Decision Tree" = perf.boom,
                "Bagged Trees" = perf.bagboom,
                "Boosted Trees" = perf.boostboom,
                "XG Boosted Trees" = perf.xgboostboom,
                "RandomForest" = perf.randomforest,
                "Logistic regression" = perf.logreg,
                "LASSO regression" = perf.lasso,
                "RIDGE regression" = perf.ridge)

#Create ROC plot
ggroc(roclist, size =1.2,  alpha=0.8, legacy.axes = TRUE) +
  scale_color_manual(values=c("gold", "forestgreen", "orange", "pink", "darkturquoise","darkorchid", "red1", "blue2")) +
  geom_abline(linetype = "3313") +
  theme_classic() +
  ggtitle("ROC Curves") +
  labs(x = "False Positive Rate",
       y = "True Positive Rate",
       linetype = "Model")

#Split line types for classification and regression
 
ggroc(list(boom1 = perf.boom,
           boom2 = perf.bagboom,
           boom3 = perf.boostboom,
           boom4 = perf.xgboostboom,
           boom5 = perf.randomforest,
           reg1 = perf.logreg,
           reg2 = perf.lasso,
           reg3 = perf.ridge), 
      size =1.4, alpha=0.7,legacy.axes = TRUE, aes = c("linetype", "colour")) +
  geom_abline(linetype = "3313") +
  scale_x_continuous("False-Positive Rate") +
  scale_y_continuous("True-Positive Rate") +
  scale_color_manual(values=c("boom1" = "gold",
                              "boom2" = "forestgreen",
                              "boom3" = "orange",
                              "boom4" = "pink",
                              "boom5" = "darkturquoise",
                              "reg1" = "darkorchid",
                              "reg2" = "red1",
                              "reg3" = "blue2"), 
                     labels=c("Decision tree",
                              "Bagges tree",
                              "Boosted tree",
                              "XGBoosted tree",
                              "RandomForest",
                              "Logistic regression",
                              "LASSO regression",
                              "RIDGE regression")) +
  scale_linetype_manual(breaks = c("boom1", 
                                   "boom2",
                                   "boom3",
                                   "boom4",
                                   "boom5",
                                   "reg1",
                                   "reg2",
                                   "reg3"),
                        values=c("solid",
                                 "solid",
                                 "solid",
                                 "solid",
                                 "solid",
                                 "longdash",
                                 "longdash",
                                 "longdash"), 
                        labels=c("Decision tree",
                              "Bagges tree",
                              "Boosted tree",
                              "XGBoosted tree",
                              "RandomForest",
                              "Logistic regression",
                              "LASSO regression",
                              "RIDGE regression")) +
    theme_classic() +
  ggtitle("ROC Curves") +
  labs(x = "False Positive Rate",
       y = "True Positive Rate")

roclist %>% 
  map(~tibble(AUC = .x$auc)) %>% 
  bind_rows(.id = "name") -> data.auc

data.auc %>% 
  mutate(label_long=paste0(name," , AUC = ",paste(round(AUC,2))),
         label_AUC=paste0("AUC = ",paste(round(AUC,2)))) -> data.labels


ggroc(roclist, size = 1.8) +
  geom_line(size = 2, color = "#b80f0a") +
  facet_wrap(~name, ncol = 4) +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(legend.position="",plot.title = element_text(size=20)) +
  labs(title = "ROC curves for the different models") +
    theme(strip.text.x = element_text(size = 13))
    
#Compare models for significant statistical difference

class_models <- c('Decision Tree', 'Bagged Trees', 'Boosted Trees','XG Boosted Trees', 'RandomForest')
reg_models <- c('Logistic regression', 'LASSO regression', 'RIDGE regression')

filter_class <- total.matrix %>% 
  filter(Algorithm %in% class_models) %>% 
  mutate(Accuracy = as.numeric(Accuracy)) %>% 
  select(-c(Algorithm))

filter_reg <- total.matrix %>% 
  filter(Algorithm %in% reg_models) %>% 
  mutate(Accuracy = as.numeric(Accuracy)) %>% 
  select(-c(Algorithm))

wilcox.test(as_vector(filter_class$Accuracy), as_vector(filter_reg$Accuracy))
wilcox.test(as_vector(filter_class$Sensitivity), as_vector(filter_reg$Sensitivity))
wilcox.test(as_vector(filter_class$Specificity), as_vector(filter_reg$Specificity))
wilcox.test(as_vector(filter_class$AUC), as_vector(filter_reg$AUC))
wilcox.test(as_vector(filter_class$PPV), as_vector(filter_reg$PPV))
wilcox.test(as_vector(filter_class$NPV), as_vector(filter_reg$NPV))
