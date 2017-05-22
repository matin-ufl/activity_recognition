#-#############################################
# Results in the manuscript.
#      << Sedentary >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################
setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")
source("f01_classification_functions.R")
source("f02_staudenmayer_functions.R")

# Loading the datasets -------------------

# Stat features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/stat_051017.Rdata")

# BoW features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/BoW_051017.Rdata")

# Making sure the given dataset has the right column names
df_bow <- bow.df[, -c(1:3, 36:37)]
df_bow$class.label <- factor(bow.df$class.sedentary, levels = c("Yes", "No"))
df_stat <- stat.df[, -c(1:2, 10:12)]
df_stat$class.label <- factor(stat.df$class.sedentary, levels = c("Yes", "No"))

# Results on statistical variables
stat.staudenmayer <- Staudenmayer.sedentary.decisionTree(df_stat)
stat.randomForest <- loocv.RandomForest(df_stat, ntree = 5000)
stat.svm <- loocv.SVM(df_stat)
stat.dt <- loocv.DecisionTree(df_stat)
print.classificationStatistics(stat.staudenmayer, "Stat + Staudenmayer")
print.classificationStatistics(stat.randomForest, "Stat + RF")
print.classificationStatistics(stat.svm, "Stat + SVM")
print.classificationStatistics(stat.dt, "Stat + DT")


# Results on BoW variables
bow.randomForest <- loocv.RandomForest(df_bow, ntree = 5000)
bow.svm <- loocv.SVM(df_bow)
bow.dt <- loocv.DecisionTree(df_bow)
print.classificationStatistics(bow.randomForest, "BoW + RF")
print.classificationStatistics(bow.svm, "BoW + SVM")
print.classificationStatistics(bow.dt, "BoW + DT")
