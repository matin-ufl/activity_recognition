#-#############################################
# Results in the manuscript.
#      << Sedentary - sub categories >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################
setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")
source("f03_sedentary_subcategories.R")

# Loading the datasets -------------------

# Stat features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/stat_051017.Rdata")

# BoW features
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Analysis Datasets/BoW_051017.Rdata")

# Making sure the given dataset has the right column names
df_bow <- bow.df[bow.df$class.sedentary == "Yes", c(2, 4:35)]
df_bow$class.label <- factor(df_bow$Task, levels = c("COMPUTER WORK", "TV WATCHING", "STANDING STILL"))
df_bow <- df_bow[, -1]
df_stat <- stat.df[stat.df$class.sedentary == "Yes", -c(1, 10:12)]
df_stat$class.label <- factor(df_stat$Task, levels = c("COMPUTER WORK", "TV WATCHING", "STANDING STILL"))
df_stat <- df_stat[, -1]

# Results on statistical variables
stat.randomForest <- loocv.RandomForest(df_stat, ntree = 5000)
table(stat.randomForest)
print.subCategory.classificationStatistics(stat.randomForest, "Stat + RF")

# Results on BoW variables
bow.randomForest <- loocv.RandomForest(df_bow, ntree = 5000, mtry = 3)
table(bow.randomForest)
print.subCategory.classificationStatistics(bow.randomForest, "BoW + RF")
