# Archive of code used for executing the scripts of the PURE study

This repository accompanies the PURE study: Evaluating Machine Learning Algorithms to Predict 30-Day Unplanned REadmission (PURE) in Urology Patients

Version 1.0, 30-01-2023

Abstract
Unplanned hospital readmissions are serious medical adverse events, stressful to patients, and expensive for hospitals. This study aims to develop a probability calculator to predict unplanned readmissions (PURE) within 30-days after discharge from the department of Urology, and evaluate the respective diagnostic performance characteristics of the PURE probability calculator developed with machine learning (ML) algorithms comparing regression versus classification algorithms.
Eight ML models (i.e. logistic regression, LASSO regression, RIDGE regression, decision tree, bagged trees, boosted trees, XGBoost trees, RandomForest) were trained on 5.323 unique patients with 52 different features, and evaluated on diagnostic performance of PURE within 30 days of discharge from the department of Urology.
Our main findings were that performances from classification to regression algorithms had good AUC scores (0.62-0.82), and classification algorithms showed a stronger overall performance as compared to models trained with regression algorithms. Tuning the best model, XGBoost, resulted in an accuracy of 0.83, sensitivity of 0.86, specificity of 0.57, AUC of 0.81, PPV of 0.95, and a NPV of 0.31.
Classification models showed stronger performance than regression models with reliable prediction for patients with high probability of readmission, and should be considered as first choice. The tuned XGBoost model shows performance that indicates safe clinical appliance for discharge management in order to prevent an unplanned readmission at the department of Urology.

Folder Structure
The main script folder contains the applied R-code for executing all analysis perfomed in the PURE study. In this folder, subfolders were added to make a distinction of the different parts of the development cyclus where code was used in. The different parts are 1) [Pre-processing] (https://github.com/koenwelvaars/PURE_study/tree/main/Pre-processing), 2) [Modelling and evaluating] (https://github.com/koenwelvaars/PURE_study/tree/main/Modelling%20and%20evaluating), and 3) [Visualisation] (https://github.com/koenwelvaars/PURE_study/tree/main/Visualisation). Please fill in your own dataframe with names and columns when reusing this code.
