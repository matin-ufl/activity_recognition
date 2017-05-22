#################################################
# Classifying sedentary vs non-sedentary
# ______________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#################################################
setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")

# Load the dataset
load('~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/BoW_W3_D32_dtw.Rdata')
dataset.df <- bow_w3_d32_dtw.df

# Attaching the sedentary label
dataset.df$class.label <- "NO"
dataset.df$class.label[dataset.df$Task %in% c("COMPUTER WORK", "TV WATCHING", "STANDING STILL")] <- "YES"
dataset.df$class.label <- as.factor(dataset.df$class.label)


set.seed(5855)
sedentary.samples <- sample(which(dataset.df$class.label == "YES"), size = 133, replace = T)
dataset.df <- rbind(dataset.df, dataset.df[sedentary.samples, ])
rm(sedentary.samples)

# loading classifier functions
source("f01_classification_functions.R")

# Random Forest
RF.out <- myRandomForest(dataset.df[, c(1, 4:(ncol(dataset.df)))])
print.classificationStatistics(RF.out, "Random Forest")


# Does Features Selection help?-----------------------------
set.seed(5855)
library(caret)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(dataset.df[, 4:(ncol(dataset.df) - 1)], dataset.df[, ncol(dataset.df)], sizes = 1:(ncol(dataset.df) - 1), rfeControl = control)
predictors(results)
plot(results, type = c("g", "o"))
featureSelected.df <- dataset.df[, c("PID", predictors(results), "class.label")]
RF2.out <- myRandomForest(featureSelected.df)
print.classificationStatistics(RF2.out)