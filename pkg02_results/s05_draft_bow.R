# Draft for BoW
setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")
# loading BoW dataset
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/bow_normal_042517.Rdata")

library(stats)
library(ggplot2)
library(reshape2)
library(cowplot)
a <- bow.df[, c(8:14, 16)]
a$class.sedentary <- as.factor(a$class.sedentary)

# Correlation Heatmap
r <- cor(a[, -8])
heatmap.m <- melt(r)
heatmap.g <- ggplot(data = heatmap.m, aes(x = Var1, y = Var2))
heatmap.g + geom_tile(aes(fill = value), color = "white") + scale_fill_continuous(low = "green", high = "red") + labs(fill = "Correlation", x = "Variable", y = "Variable")
rm(r, heatmap.g, heatmap.m)


# Sedentary classifications -------------------------------
dataset.df <- bow.df[, c(1, 8:14, 16)]
svm.model <- mySVM(dataset.df)
print.classificationStatistics(svm.model, classifier.name = "SVM")
