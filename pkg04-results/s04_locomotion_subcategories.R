#-#############################################
# Results in the manuscript.
#      << Locomotion - sub categories >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################
setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")
source("f04_locomotion_subcategories.R")

# Loading the datasets -------------------

# Stat features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/stat_051017.Rdata")

# BoW features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/BoW_locomotion_051117.Rdata")

# Making sure the given dataset has the right column names
df_bow <- bow.df[bow.df$class.locomotion == "Yes", c(2, 4:35)]
df_bow$class.label <- factor(df_bow$Task, levels = c("LEISURE WALK", "WALKING AT RPE 1", "RAPID WALK", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT"))
df_bow <- df_bow[, -1]
df_stat <- stat.df[stat.df$class.locomotion == "Yes", -c(1, 10:12)]
df_stat$class.label <- factor(df_stat$Task, levels = c("LEISURE WALK", "WALKING AT RPE 1", "RAPID WALK", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT"))
df_stat <- df_stat[, -1]

# Results on statistical variables
stat.randomForest <- loocv.RandomForest(df_stat, ntree = 5000)
table(stat.randomForest)
print.subCategory.classificationStatistics(stat.randomForest, "Stat + RF")

# Results on BoW variables
bow.randomForest <- loocv.RandomForest(df_bow, ntree = 10000, mtry = 3)
table(bow.randomForest)
print.subCategory.classificationStatistics(bow.randomForest, "BoW + RF")