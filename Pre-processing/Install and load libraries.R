#Function to load libraries and install when necessary
pakFunctie <- function(pkg){ 
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
    if (length(new.pkg))  
       install.packages(new.pkg, dependencies = TRUE) 
    sapply(pkg, require, character.only = TRUE) 
    } 
 
 #Install and load libraries
 packages <- c(...) 
 
 #Execute function
pakFunctie(packages)

#Libraries used in this study:
#"ROCR", "mice", "VIM", "DMwR", "pscl", "magrittr", "ipred", "C50", "ggcorrplot", "factoextra", 
#"stringr", "reshape2","RColorBrewer", "plotly", "viridis", "outliers", "reshape2", "tidyverse",
#"knitr", "ggfortify", "factoextra", "readxl","smotefamily","sjPlot", "naniar", "table1", "fastDummies","randomForest"
