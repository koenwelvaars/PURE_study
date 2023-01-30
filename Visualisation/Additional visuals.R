#Load libraries and data

pakFunctie <- function(pkg){ 
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
    if (length(new.pkg))  
       install.packages(new.pkg, dependencies = TRUE) 
    sapply(pkg, require, character.only = TRUE) 
    } 
 
 packages <- c("ROCR", "mice", "VIM", "DMwR", "pscl", "magrittr", "ipred", "C50", "ggcorrplot", "factoextra", "stringr", "reshape2","RColorBrewer", "plotly", "viridis", "outliers", "reshape2", "tidyverse", "knitr", "ggfortify", "factoextra", "readxl","smotefamily","sjPlot", "naniar", "table1", "webshot") 
 pakFunctie(packages)

df <- read_csv("df.csv")
str(df)

#Create plot for distribution per numerical feature

df %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(x=value)) +
  geom_density(alpha = 0.2, color = "dodgerblue4", fill = "dodgerblue1")  +
  facet_wrap(~key, scales = "free") +
  theme_classic() +
  ggtitle("Distribution numeric features")

#Function to calculate p-value for patient characteristics table
pvalue <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform an ANOVA
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#Format the data
df %>% 
  select(-c(outcome)) %>% 
  colnames %>% 
  paste(collapse = "+") 

#Paste formatted data ouput as string below
table1(~ string | outcome,
       data=df,
       overall =  "Total", 
       extra.col=list(`P-value`=pvalue),
       extra.col.pos = 3)

#Create flowchart for population

install.packages("DiagrammeR")
library(DiagrammeR)

grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']


      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab8;
      tab4 -> tab7;
      tab7 -> tab8;

      }

      [1]: 'Observations extracted from medical database n=7.570'
      [2]: 'Imputation of missing data n=7.570'
      [3]: 'Feature selection n=7.570'
      [4]: 'Splitting data in test (30%) and train set (70%)'
      [5]: 'Train set n=5.251 '
      [6]: 'SMOTE n=3.004'
      [7]: 'Test set n=2.319'
      [8]: 'Study sample n=5.323'
      ")
      
      
