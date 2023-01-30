# Outlier analysis
# Function plotting for concentration between x and y
outlier_hexplot <- function(column_x, column_y) {
  
  hex_df <- ggplot(df, aes_string(column_x, column_y))
  hexplot <- hex_df + 
    geom_hex(bins = 30) +
    scale_fill_viridis(option = "plasma") + 
    theme_classic() +
    ggtitle("Concentratie van data") +
    xlab(column_x) +
    ylab(column_y) +
    labs(fill = "Density") 
  hexplot
}

outlier_hexplot("x", "y")

# Table to look up on outliers
outliers <- df %>%
  dplyr::select_if(is.numeric)

max_outlier <- outlier(outliers)
z_score_max <- round(scores(max_outlier),3)
chi2_max <- round(scores(max_outlier, type = "chisq"),3)

min_outlier <- outlier(outliers,opposite=TRUE)
z_score_min <- round(scores(min_outlier),3)
chi2_min <- round(scores(min_outlier, type = "chisq"),3)

table_outliers <- as.data.frame(cbind(min_outlier,z_score_min, chi2_min,
                                        max_outlier,z_score_max, chi2_max))
colnames(table_outliers) <- c("min_outlier", "min_zscore", "min_chi2", 
                                "max_outlier", "max_zscore", "max_chi2")
table_outliers

# Replace outliers with missings per feature
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Count of outliers indentified:", na2 - na1, "n")
  cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean with outliers:", round(m1, 2), "n")
  cat("Mean without outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

# Calling the function
outlierKD(df, feature)
