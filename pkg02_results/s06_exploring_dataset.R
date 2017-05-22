#######################################################
# Summary statistics of the dataset
# _____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#######################################################

# Load the dataset
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/BoW_W3_D32.Rdata")

# Check which variables should be present for summary statistics
colnames(bow_w3_d32.df)

# The dataset for summary statistics should contain <Task, variables>
# Task is a character-column and the rest are numeric.
dataset.df <- bow_w3_d32.df[, 2:ncol(bow_w3_d32.df)]

variables <- colnames(dataset.df)[2:ncol(dataset.df)]
for(task in levels(as.factor(dataset.df$Task))) {
     outStr <- paste(task, "&")
     for(variable in variables) {
          outStr <- paste(outStr, variable, ": ", round(mean(dataset.df[dataset.df$Task == task, variable], na.rm = T), digits = 2), " (",
                          round(sd(dataset.df[dataset.df$Task == task, variable], na.rm = T), digits = 2), ") & ", sep = "")
     }
     print(outStr)
}
rm(outStr, task, variable, variables)


# Correlation heatmap
library(ggplot2)
library(cowplot)
library(reshape2)

# We should remove 'Task' and "MET' before checking for correlations
r <- cor(dataset.df[, -c(1, 2)])
plot.df <- melt(r); colnames(plot.df) <- c("Variable", "variable", "correlation")
plot.df$Correlation <- abs(plot.df$correlation)
g <- ggplot(data = plot.df, aes(x = variable, y = Variable)) + geom_tile(aes(fill = Correlation), color = "white") + scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 0.5)
