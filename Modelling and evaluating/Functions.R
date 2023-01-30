#------------------------------------------------------------------------------------
#File containing all data modeling functions
#------------------------------------------------------------------------------------

#load all libraries
pakFunctie <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
  if (length(new.pkg))  
    install.packages(new.pkg, dependencies = TRUE) 
  sapply(pkg, require, character.only = TRUE) 
} 

#------------------------------------------------------------------------------------
#Create a decision tree with GINI index
#------------------------------------------------------------------------------------

beslisboom <- function(df, x, y){
  
  crossval <- trainControl(method = "cv", number = 5)
  x_namen <- colnames(select(df, -c(y)))           
  x <- paste(x_namen, collapse = " + ")               
  yx_formule <- as.formula(paste(y, x, sep = " ~ ")) 
  
  set.seed(84)  
  train(yx_formule, 
        data = df, 
        method = "rpart", 
        parms = list(split="gini"),
        trControl = crossval,
        tuneLength = 10)
}

#Plot the decision tree
plot.beslisboom <- function(finalmodel){
  prp(finalmodel, 
      box.palette = "Purples", 
      tweak = 1,
      main = "Standaard beslisboom")
}

#------------------------------------------------------------------------------------
#Machine Learning decision tree (bag/boost)
#------------------------------------------------------------------------------------

ml.beslisboom <- function(df, x, y, method){
  
  crossval <- trainControl(method = "cv", number = 5)
  x_namen <- colnames(select(df, -c(y)))           
  x <- paste(x_namen, collapse = " + ")               
  yx_formule <- as.formula(paste(y, x, sep = " ~ ")) 
  
  set.seed(84)
  beslisboom <- train(yx_formule, 
                    data = df, 
                    method = method,
                    trControl = crossval,
                    verbose = FALSE)
}

#------------------------------------------------------------------------------------
#Random Forest
#------------------------------------------------------------------------------------

ml.randomforest <- function(df, x, y, ntree){
  if(missing(ntree)){
    x_namen <- colnames(select(df, -c(y)))           
    x <- paste(x_namen, collapse = " + ")               
    yx_formule <- as.formula(paste(y, x, sep = " ~ ")) 
    
    set.seed(84)
    beslisboom <- train(yx_formule,
                        data = df,
                        method = 'rf',
                        ntree=500,
                        trControl = trainControl(method = "repeatedcv",
                                                 number = 5,
                                                 savePredictions = "final",
                                                 classProbs = T),
                        tuneLength = 1,
                        importance = TRUE)
    
  } 
  else{x_namen <- colnames(select(df, -c(y)))           
  x <- paste(x_namen, collapse = " + ")               
  yx_formule <- as.formula(paste(y, x, sep = " ~ ")) 
  
  set.seed(84)
  beslisboom <- train(yx_formule,
                      data = df,
                      method = 'rf',
                      ntree=ntree,
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               savePredictions = "final",
                                               classProbs = T),
                      tuneLength = 1,
                      importance = TRUE)
  
  }
}
#------------------------------------------------------------------------------------
#Calculate metrics for RandomForest
#------------------------------------------------------------------------------------

rf.uitkomst.model <- function(model, testdata, y){
  
  
  pred_model <- predict(model, testdata)
  uitkomsten_model <- confusionMatrix(pred_model, y)
  
  y.test = y
  auc_model <- ROCR::prediction(as.numeric(pred_model), as.numeric(y.test))
  
  sens.rf <- round(uitkomsten_model[["byClass"]][["Sensitivity"]],2)
  spec.rf <- round(uitkomsten_model[["byClass"]][["Specificity"]],2)
  auc.rf <- round(performance(auc_model, measure = "auc")@y.values[[1]],3)
  ppv.rf <- round(uitkomsten_model[["byClass"]][["Pos Pred Value"]],2)
  npv.rf <- round(uitkomsten_model[["byClass"]][["Neg Pred Value"]],2)

  rijnaam <- c("Prestatie")
  data.frame(row.names = rijnaam,
             sens.rf,                
             spec.rf,
             auc.rf,
             ppv.rf,
             npv.rf)
}


#------------------------------------------------------------------------------------
#ROC curve metrics
#------------------------------------------------------------------------------------

roc.boom <- function(uitkomst, model, testdata){
  
    pred.beslisboom <- predict(model, newdata = testdata,type = "prob")[,2]
    pred.beslisboom <- as.numeric(pred.beslisboom)
    rocobj <- roc(uitkomst, pred.beslisboom)
    
  }

roc.randomforest <- function(model, testdata, y){
  rf_p <- predict(model, testdata, type = "prob")[,2]
  rocobj <- roc(y, rf_p)
}


roc.regressie <- function(model, testdata, test_y){
  roc.pred.log.reg <- predict(model, newdata = testdata, type = "raw")
  rocobj <- roc(test_y, roc.pred.log.reg)
}

roc.lassoridge <- function(x_train, x_test, y, y_train, y_test, alpha){
  
  x_namen <- colnames(select(x_train, -c(y)))           
  x <- paste(x_namen, collapse = " + ")               
  yx_formule.train <- as.formula(paste(y, x, sep = " ~ ")) 
  yx_formule.test <- as.formula(paste(y, x, sep = " ~ ")) 
  
  set.seed(84)
  lassoridge.train <- model.matrix(yx_formule.train, x_train)
  lassoridge.test <- model.matrix(yx_formule.test, x_test)
  
  #Model trainen
  set.seed(84)
  lassoridgetrain <- cv.glmnet(lassoridge.train, y_train, alpha = alpha, 
                               family = "binomial")
  
  pred.test <- predict(lassoridgetrain, newx = lassoridge.test,type = "response")
  pred.test <- as.numeric(pred.test)
  rocobj <- roc(y_test, pred.test)
  
}
#------------------------------------------------------------------------------------
#Calculate metrics for decision trees
#------------------------------------------------------------------------------------

uitkomst.model <- function(uitkomst, model, testdata, type = "prob"){
  
  if(type != "prob") {
  
  pred.beslisboom <- predict(model, newdata = testdata, type = "raw")
  confmat.beslisboom <-confusionMatrix(as.factor(pred.beslisboom), as.factor(uitkomst))
  auc.beslisboom <- predict(model, type = "prob", newdata = testdata)
  boompred <- ROCR::prediction(auc.beslisboom[,2], uitkomst)
  boomperf <- performance(boompred, "tpr", "fpr")
  boomauc <- performance(boompred, "auc")
  
  acc.boom <- round(confmat.beslisboom$byClass['Balanced Accuracy'],2)
  sens.boom <- round(confmat.beslisboom$byClass['Sensitivity'],2)
  spec.boom <- round(confmat.beslisboom$byClass['Specificity'],2)
  auc.boom <- round(as.numeric(boomauc@y.values),2)
  ppv.boom <- round(confmat.beslisboom$byClass["Pos Pred Value"],2)
  npv.boom <- round(confmat.beslisboom$byClass["Neg Pred Value"],2)

  rijnaam <- c("Prestatie")
  data.frame(row.names = rijnaam,
             acc.boom,
             sens.boom,
             spec.boom,
             auc.boom,
             ppv.boom,
             npv.boom)
  }
  else {
    pred.beslisboom <- predict(model, newdata = testdata)
    confmat.beslisboom <-confusionMatrix(as.factor(pred.beslisboom), as.factor(uitkomst))
    auc.beslisboom <- predict(model, type = "prob", newdata = testdata)
    boompred <- ROCR::prediction(auc.beslisboom[,2], uitkomst)
    boomperf <- performance(boompred, "tpr", "fpr")
    boomauc <- performance(boompred, "auc")
    
    acc.boom <- round(confmat.beslisboom$byClass['Balanced Accuracy'],2)
    sens.boom <- round(confmat.beslisboom$byClass['Sensitivity'],2)
    spec.boom <- round(confmat.beslisboom$byClass['Specificity'],2)
    auc.boom <- round(as.numeric(boomauc@y.values),2)
    ppv.boom <- round(confmat.beslisboom$byClass["Pos Pred Value"],2)
    npv.boom <- round(confmat.beslisboom$byClass["Neg Pred Value"],2)
    
    rijnaam <- c("Prestatie")
    data.frame(row.names = rijnaam,
               acc.boom,
               sens.boom,
               spec.boom,
               auc.boom,
               ppv.boom,
               npv.boom)
  }
}

#------------------------------------------------------------------------------------
#Train a logreg model
#------------------------------------------------------------------------------------

ml.regressie <- function(df, x, y){
    trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                          preProcOptions = c("center", "scale"))
    
    x_namen <- colnames(select(df, -c(y)))           
    x <- paste(x_namen, collapse = " + ")               
    yx_formule <- as.formula(paste(y, x, sep = " ~ "))
    
  # Simpele logistische regressie
  set.seed(84)
  log.reg <- train(yx_formule, 
                   data = df, 
                   method = "glm", 
                   family = "binomial",
                   trControl=trctrl)
}
  
#------------------------------------------------------------------------------------
#Calculate metrics for logreg model
#------------------------------------------------------------------------------------

uitkomst.regressie <- function(model, testdata, test_y){
  pred.log.reg <- predict(model, newdata = testdata)
  cm.log.reg <- confusionMatrix(as.factor(round(pred.log.reg)), as.factor(round(test_y)))
  
  # samenvatting voor totaaloverzicht
  sens.log <- round(cm.log.reg$byClass['Sensitivity'],2)
  spec.log <- round(cm.log.reg$byClass['Specificity'],2)
  acc.log <- sprintf("%0.2f",(sens.log + spec.log)/2)
  
  prediction.log <- ROCR::prediction(as.numeric(pred.log.reg),test_y)
  auc.log <- unlist(performance(prediction.log, measure = "auc")@y.values)
  auc.log <- round(auc.log, 2)
  perflog <- performance(prediction.log, "tpr", "fpr")
  ppv.log <- round(cm.log.reg$byClass["Pos Pred Value"],2)
  npv.log <- round(cm.log.reg$byClass["Neg Pred Value"],2)

  rijnaam <- c("Prestatie")
  data.frame(row.names = rijnaam,
             acc.log,                
             sens.log,
             spec.log,
             auc.log,
             ppv.log,
             npv.log)
}

#------------------------------------------------------------------------------------
#Train LASSO/RIDGE regression model
#------------------------------------------------------------------------------------

ml.lassoridge <- function(x_train, x_test, y, y_train, y_test, alpha){
  
  x_namen <- colnames(select(x_train, -c(y)))           
  x <- paste(x_namen, collapse = " + ")               
  yx_formule.train <- as.formula(paste(y, x, sep = " ~ ")) 
  yx_formule.test <- as.formula(paste(y, x, sep = " ~ ")) 
  
set.seed(84)
lassoridge.train <- model.matrix(yx_formule.train, x_train)
lassoridge.test <- model.matrix(yx_formule.test, x_test)

#Model trainen
set.seed(84)
lassoridgetrain <- cv.glmnet(lassoridge.train, y_train, alpha = alpha,
                        family = "binomial", standardize = TRUE)

coeffs <- coef(lassoridgetrain)

pred.test <- predict(lassoridgetrain, newx = lassoridge.test)
predclass.test <- ifelse(pred.test > 0.5, 1,0)
cm.lassoridge <- confusionMatrix(as.factor(predclass.test), as.factor(y_test))

sens.lassoridge <- round(cm.lassoridge$byClass['Sensitivity'],2)
spec.lassoridge <- round(cm.lassoridge$byClass['Specificity'],2)
acc.lassoridge <- sprintf("%0.2f",(sens.lassoridge + spec.lassoridge)/2)

auclassoridge <- ROCR::prediction(predclass.test,y_test)
perflassoridge <- performance(auclassoridge, "tpr", "fpr")
auc.lassoridge <- unlist(performance(auclassoridge, measure = "auc")@y.values)
auc.lassoridge <- round(auc.lassoridge,2)
ppv.lassoridge <- round(cm.lassoridge$byClass["Pos Pred Value"],2)
npv.lassoridge <- round(cm.lassoridge$byClass["Neg Pred Value"],2)

rijnaam <- c("Prestatie")
data.frame(row.names = rijnaam,
           acc.lassoridge,
           sens.lassoridge,
           spec.lassoridge,
           auc.lassoridge,
           ppv.lassoridge,
           npv.lassoridge)
}
