#Read data
df <- read_csv("...")

#Check for current balance of outcome data
table(df$outcome)
round(prop.table(table(df$outcome)),1)

#Split data 70/30 and only use train data for SMOTE
set.seed(84)
index <- sample(seq(1, 2), size = nrow(df), replace = TRUE, prob = c(.7, .3))
df.train <- df[index == 1,]
df.test <- df[index == 2,]

#Save test data
write.csv(df.test, "test_data.csv",row.names=FALSE)

#Function for SMOTE
balanceren_df <- function(df, balance_minor,balance_major){
  
  n_minor <- sum(df$outcome == "Yes")

  factor_minor <- (((nrow(df) * balance_minor) - n_minor) / n_minor) * 100
  factor_major <- ((nrow(df) * balance_major) / (n_minor * (factor_minor / 100))) * 100

  set.seed(84)
  DMwR::SMOTE("outcome"~., 
              df, 
              perc.over = factor_minor,    #formule = (n minor * factor minor) + n minor
              k = 5,
              perc.under = factor_major)   #formule = (n minor * factor minor) * factor major
  }

#Execute custom SMOTE function
smote_df <- balanceren_df(df = df.train,
                            balans_minor = 0.30,
                            balans_major = 0.70)
                            
#Check for new balance
table(smote_df$outcome)
round(prop.table(table(smote_df$outcome)),2)

#Round numeric data after SMOTE
smote_df %>% 
  mutate_at(vars(c(...)), 
            funs(round(., 0))) %>% 
  mutate_at(vars(c(...)), 
            funs(round(., 1))) 
  
#Save SMOTED data for training
write.csv(df_70smote_df_30, "train_data.csv",row.names=FALSE)
