#-#############################################
# Results in the manuscript.
#      << 2- and 3-step MET estimation >>
# ____________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#-#############################################

setwd("~/Workspaces/R workspace/Activity Recognition/pkg04-results/")

# Training sedentary classifier ------------------------------------------
library(randomForest)
load("temp_sedentary.Rdata")

# stat variables =======================
df_stat <- stat.df[, -c(1:2, 10:12)]
df_stat$class.label <- factor(stat.df$class.sedentary, levels = c("Yes", "No"))
set.seed(5855)
stat.sedentary.classifier <- randomForest(data = df_stat, class.label ~ ., ntree = 10000, mtry = 2)
#predicted.stat.sedentary <- as.character(predict(stat.sedentary.classifier, newdata = df_stat[, -ncol(df_stat)]))

# BoW variables ========================
df_bow <- bow.df[, -c(1:3, 36:37)]
df_bow$class.label <- factor(bow.df$class.sedentary, levels = c("Yes", "No"))
set.seed(5855)
bow.sedentary.classifier <- randomForest(data = df_bow, class.label ~ ., ntree = 10000, mtry = 3)
# predicted.bow.sedentary <- as.character(predict(bow.sedentary.classifier, newdata = df_bow[, -ncol(df_bow)]))

rm(bow.df, df_bow, df_stat, stat.df)

# Training locomotion classifier ------------------------------------------
load("temp_locomotion.Rdata")

# stat variables =======================
set.seed(5855)
stat.locomotion.classifier <- randomForest(data = df_stat, class.label ~ ., ntree = 10000, mtry = 2)
#predicted.stat.locomotion <- as.character(predict(stat.locomotion.classifier, newdata = df_stat[, -ncol(df_stat)]))

# BoW variables ========================
set.seed(5855)
bow.locomotion.classifier <- randomForest(data = df_bow, class.label ~ ., ntree = 10000, mtry = 3)
#predicted.bow.locomotion <- as.character(predict(bow.locomotion.classifier, newdata = df_bow[, -ncol(df_bow)]))

rm(bow.df, df_bow, df_stat, stat.df)

# MET evaluation using sedentary and locomotion variables ----------------
load("temp_met.Rdata")
predicted.stat.sedentary <- as.character(predict(stat.sedentary.classifier, newdata = df_stat[, -ncol(df_stat)]))
predicted.stat.locomotion <- as.character(predict(stat.locomotion.classifier, newdata = df_stat[, -ncol(df_stat)]))
df_stat$sedentary <- 0
df_stat$sedentary[predicted.stat.sedentary == "Yes"] <- 1
df_stat$locomotion <- 0
df_stat$locomotion[predicted.stat.locomotion == "Yes"] <- 1
rm(predicted.stat.locomotion, predicted.stat.sedentary)

predicted.bow.sedentary <- as.character(predict(bow.sedentary.classifier, newdata = df_bow[, -ncol(df_bow)]))
predicted.bow.locomotion <- as.character(predict(bow.locomotion.classifier, newdata = df_bow[, -ncol(df_bow)]))
df_bow$sedentary <- 0
df_bow$sedentary[predicted.bow.sedentary == "Yes"] <- 1
df_bow$locomotion <- 0
df_bow$locomotion[predicted.bow.locomotion == "Yes"] <- 1
rm(predicted.bow.locomotion, predicted.bow.sedentary)

save(df_bow, df_stat, file = "temp_met_sed_loc.Rdata")
rm(list = ls())


# Training sedentary subcategory classifier -------------------------------
load("temp_sedentary_subcategories.Rdata")

# stat variables =======================
set.seed(5855)
stat.subSedentary.classifier <- randomForest(data = df_stat, class.label ~ ., ntree = 10000, mtry = 2)
# predicted.stat.subSedentary <- as.character(predict(stat.subSedentary.classifier, newdata = df_stat[, -ncol(df_stat)]))

# BoW variables ========================
set.seed(5855)
bow.subSedentary.classifier <- randomForest(data = df_bow, class.label ~ ., ntree = 10000, mtry = 3)
# predicted.bow.subSedentary <- as.character(predict(bow.subSedentary.classifier, newdata = df_bow[, -ncol(df_bow)]))

rm(df_bow, df_stat)

# Training locomotion subcategory classifier -----------------------------
load("temp_locomotion_subcategories.Rdata")

# stat variables =======================
set.seed(5855)
stat.subLocomotion.classifier <- randomForest(data = df_stat, class.label ~ ., ntree = 10000, mtry = 2)
# predicted.stat.subLocomotion <- as.character(predict(stat.subLocomotion.classifier, newdata = df_stat[, -ncol(df_stat)]))

# BoW variables ========================
set.seed(5855)
bow.subLocomotion.classifier <- randomForest(data = df_bow, class.label ~ ., ntree = 10000, mtry = 3)
# predicted.bow.subLocomotion <- as.character(predict(bow.subLocomotion.classifier, newdata = df_bow[, -ncol(df_bow)]))

rm(bow.df, df_bow, df_stat, stat.df)


# MET evaluation using sedentary and locomotion subcategories ----------------
load("temp_met_sed_loc.Rdata")
nonSedentaryIdx <- df_stat$sedentary == 0
predicted.stat.subSedentary <- as.character(predict(stat.subSedentary.classifier, newdata = df_stat[, 1:7]))
df_stat$CW <- 0
df_stat$CW[predicted.stat.subSedentary == "COMPUTER WORK"] <- 1
df_stat$CW[nonSedentaryIdx] <- 0
df_stat$SS <- 0
df_stat$SS[predicted.stat.subSedentary == "STANDING STILL"] <- 1
df_stat$SS[nonSedentaryIdx] <- 0
df_stat$TW <- 0
df_stat$TW[predicted.stat.subSedentary == "TV WATCHING"] <- 1
df_stat$TW[nonSedentaryIdx] <- 0
rm(predicted.stat.subSedentary, nonSedentaryIdx)

nonLocomotionIdx <- df_stat$locomotion == 0
predicted.stat.subLocomotion <- as.character(predict(stat.subLocomotion.classifier, newdata = df_stat[, 1:7]))
df_stat$LW <- 0
df_stat$LW[predicted.stat.subLocomotion == "LEISURE WALK"] <- 1
df_stat$LW[nonLocomotionIdx] <- 0
df_stat$RW <- 0
df_stat$RW[predicted.stat.subLocomotion == "RAPID WALK"] <- 1
df_stat$RW[nonLocomotionIdx] <- 0
df_stat$SA <- 0
df_stat$SA[predicted.stat.subLocomotion == "STAIR ASCENT"] <- 1
df_stat$SA[nonLocomotionIdx] <- 0
df_stat$SD <- 0
df_stat$SD[predicted.stat.subLocomotion == "STAIR DESCENT"] <- 1
df_stat$SD[nonLocomotionIdx] <- 0
df_stat$WAR1 <- 0
df_stat$WAR1[predicted.stat.subLocomotion == "WALKING AT RPE 1"] <- 1
df_stat$WAR1[nonLocomotionIdx] <- 0
df_stat$WAR5 <- 0
df_stat$WAR5[predicted.stat.subLocomotion == "WALKING AT RPE 5"] <- 1
df_stat$WAR5[nonLocomotionIdx] <- 0
rm(predicted.stat.subLocomotion, nonLocomotionIdx)

nonSedentaryIdx <- df_bow$sedentary == 0
predicted.bow.subSedentary <- as.character(predict(bow.subSedentary.classifier, newdata = df_bow[, 1:32]))
df_bow$CW <- 0
df_bow$CW[predicted.bow.subSedentary == "COMPUTER WORK"] <- 1
df_bow$CW[nonSedentaryIdx] <- 0
df_bow$SS <- 0
df_bow$SS[predicted.bow.subSedentary == "STANDING STILL"] <- 1
df_bow$SS[nonSedentaryIdx] <- 0
df_bow$TW <- 0
df_bow$TW[predicted.bow.subSedentary == "TV WATCHING"] <- 1
df_bow$TW[nonSedentaryIdx] <- 0
rm(predicted.bow.subSedentary, nonSedentaryIdx)

nonLocomotionIdx <- df_bow$locomotion == 0
predicted.bow.subLocomotion <- as.character(predict(bow.subLocomotion.classifier, newdata = df_bow[, 1:32]))
df_bow$LW <- 0
df_bow$LW[predicted.bow.subLocomotion == "LEISURE WALK"] <- 1
df_bow$LW[nonLocomotionIdx] <- 0
df_bow$RW <- 0
df_bow$RW[predicted.bow.subLocomotion == "RAPID WALK"] <- 1
df_bow$RW[nonLocomotionIdx] <- 0
df_bow$SA <- 0
df_bow$SA[predicted.bow.subLocomotion == "STAIR ASCENT"] <- 1
df_bow$SA[nonLocomotionIdx] <- 0
df_bow$SD <- 0
df_bow$SD[predicted.bow.subLocomotion == "STAIR DESCENT"] <- 1
df_bow$SD[nonLocomotionIdx] <- 0
df_bow$WAR1 <- 0
df_bow$WAR1[predicted.bow.subLocomotion == "WALKING AT RPE 1"] <- 1
df_bow$WAR1[nonLocomotionIdx] <- 0
df_bow$WAR5 <- 0
df_bow$WAR5[predicted.bow.subLocomotion == "WALKING AT RPE 5"] <- 1
df_bow$WAR5[nonLocomotionIdx] <- 0
rm(predicted.bow.subLocomotion, nonLocomotionIdx)

rm(bow.subLocomotion.classifier, bow.subSedentary.classifier, stat.subLocomotion.classifier, stat.subSedentary.classifier)
save(df_bow, df_stat, file = "temp_met_subSed_subLoc.Rdata")
