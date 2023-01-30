#First train a model to perform feature engineering with

set.seed(84)
rf.model <- randomForest(outcome ~ .,
                         data=df,
                         ntree= 2500,
                         keep.forest=FALSE,
                         importance=TRUE)

#Extract feature importance from RandomForest classifier
importance <- as.data.frame(varImpPlot(rf.model, type = 1))

#Format the data
data_imp <- importance %>%
  arrange(MeanDecreaseAccuracy) %>%
  mutate(x = colnames(test_df)) %>%
  mutate(x = factor(x,x)) %>%
  mutate(y = importantie[,1]) %>%
  mutate(y = as.integer(y)) %>%
  select(x,y)

#Select the only features with more or equal to 10% importance 
data_imp <- data_imp %>%
  arrange(y) %>%
   mutate(x=factor(x,x)) %>%
   filter(y >= 10)

#Plot the data
data_imp %>%
  mutate(kleur = ifelse(y >= 10,"#b80f0a","#0AB3B8")) %>%
  ggplot(aes(x=x, y=y)) +
  geom_segment(aes(x=x, xend=x, y=0, yend=y, colour = kleur),  size=8) +
  scale_colour_identity() +
  theme_light() +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = 9.5),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("Results feature engineering using a RandomForest") +
  xlab("Features") + ylab("Importance (Mean Decrease in Accuracy)")

#Select only features with selected criteria (>=10 % importance)
imp_vars <- as.vector(data_imp[,1])
df <- df %>%
  select(contains(imp_vars),outcome)
