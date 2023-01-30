
#Check data for missing values
sum(is.na(df))
colSums(is.na(df))

#Create subset for data with missing values
missings <- df %>% 
  select(...) %>%
  miss_var_summary() %>%
  filter(n_miss > 0) %>% 
  rename(Feature = feature,
         Count = n_miss,
         Percentage = pct_miss)

missing_table$Percentage <- format(round(missing_table$Percentage,digits = 1),nsmall = 1)

#Write table with missing values per feature
tab_df(missing_table,
       file = "table2_missings.doc")
       
#Plot missing values with density per feature
aggr(missende, col=c('white','#b80f0a'),
                  numbers=TRUE, sortVars=TRUE,
                  prop = c(TRUE, FALSE),
                  labels=names(missings), cex.axis=0.8,oma = c(7,5,5,1),
                  gap=3, ylab=c("Proportion of missing values per feature",
                                "Pattern of missing (red) and non-missing (white) values"),
     title(main = "Missing values and pattern"))

#Impute missings with MICE
imputed_df <- mice(df, maxit = 100, method = 'pmm', seed = 84)

#Check process of imputing
summary(imputed_df)
densityplot(imputed_df)

#Save inmputed and complete data to new dataset and write to .csv 
complete_df <- mice::complete(imputed_df)
write.csv(complete_df, "imputed_data.csv",row.names=FALSE)
