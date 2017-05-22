# Classification results
setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/wrist_features_042417.Rdata")

# Sedentary classification --------------------------

# Discarding unnecessary columns
dataset.df <- activityRecognition.df[, c(1, 5:11, 13)]

# Converting the class-label column to factor
dataset.df$class.locomotion <- as.factor(toupper(dataset.df$class.locomotion))


source("f01_classification_functions.R")
source("f02_staudenmayer_functions.R")

# Staudenmayer
out1 <- Staudenmayer.locomotion.decisionTree(dataset.df[, -1])
print.classificationStatistics(out1, "Staudenmayer Sedentary")


# Random Forest
out2 <- myRandomForest(dataset.df)
print.classificationStatistics(out2, "Zhang + RF")
