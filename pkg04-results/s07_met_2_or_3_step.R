#-#############################################
# Results in the manuscript.
#      << MET estimation - 2-step or 3-step >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################

setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")
source("f05_MET_estimation.R")

# Sedentary and Locomotion ----------------------
load("temp_met_sed_loc.Rdata")

stat.randomForest <- loocv.RandomForest(df_stat[, c(1:7, 9:10, 8)], ntree = 5000)
bow.randomForest <- loocv.RandomForest(df_bow[, c(1:32, 34:35, 33)], ntree = 5000, mtry = 3)
print.regressionStatistics(stat.randomForest, "Stat + (Sedentary, Locomotion)")
print.regressionStatistics(bow.randomForest, "BoW + (Sedentary, Locomotion)")
rm(list = ls())

# Subcategories -------------------------------
source("f05_MET_estimation.R")
load("temp_met_subSed_subLoc.Rdata")
stat.randomForest <- loocv.RandomForest(df_stat[, c(1:7, 9:ncol(df_stat), 8)], ntree = 5000)
bow.randomForest <- loocv.RandomForest(df_bow[, c(1:32, 34:ncol(df_bow), 33)], ntree = 5000, mtry = 3)
print.regressionStatistics(stat.randomForest, "Stat + (subcategories)")
print.regressionStatistics(bow.randomForest, "BoW + (subcategories)")
