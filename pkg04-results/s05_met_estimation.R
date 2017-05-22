#-#############################################
# Results in the manuscript.
#      << MET estimation >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################

setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")
source("f05_MET_estimation.R")
source("f02_staudenmayer_functions.R")

# Load datasets
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/MET_datasets_051217.Rdata")

df_stat <- stat.df[, c(3:10)]
df_bow <- bow.df[, c(4:ncol(bow.df), 3)]

stat.staudenmayer <- Staudenmayer.MET.linearRegression(df_stat)
stat.svm <- loocv.SVM(df_stat)
stat.decisitonTree <- loocv.DecisionTree(df_stat)
stat.randomForest <- loocv.RandomForest(df_stat, ntree = 5000)
print.regressionStatistics(stat.staudenmayer, "Stat + Staudenmayer")
print.regressionStatistics(stat.randomForest, "Stat + Random Forest")
print.regressionStatistics(stat.svm, "Stat + SVM")
print.regressionStatistics(stat.decisitonTree, "Stat + Decision Tree")

bow.svm <- loocv.SVM(df_bow)
bow.decisionTree <- loocv.DecisionTree(df_bow)
bow.randomForest <- loocv.RandomForest(df_bow, ntree = 5000, mtry = 3)
print.regressionStatistics(bow.randomForest, "BoW + Random Forest")
print.regressionStatistics(bow.svm, "BoW + SVM")
print.regressionStatistics(bow.decisionTree, "BoW + Decision Tree")
