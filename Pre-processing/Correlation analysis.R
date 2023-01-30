#Create dataset for correlation analysis
corr_df <- df %>%
  mutate_at(vars(c(...)), 
            function(x) 
              case_when(
                x == "No" ~ 0,
                x == "Yes" ~ 1)) %>% 
  mutate(sex = case_when(
                sex == "Male" ~ 0,
                sex == "Female" ~ 1))  %>% 
  select(-c(outcome)) %>% 
  mutate_if(is.factor,numeric)

#Create correlation matrix
r <- cor(corr_df, use="complete.obs")
round(r,2)

#Plot correlation matrix
ggcorrplot(r, 
           hc.order = TRUE, 
          type = "lower",
           tl.cex = 10)
