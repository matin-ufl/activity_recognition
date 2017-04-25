# Classification results
setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/wrist_features_042417.Rdata")

# Sedentary classification --------------------------

# Discarding unnecessary columns
dataset.df <- activityRecognition.df[, c(1, 5:11, 14)]

# Converting the class-label column to factor
dataset.df$class.sedentary <- as.factor(dataset.df$class.sedentary)

# Leave-one-subject-out-cross-validation
set.seed(5855)
sample.ppts <- sample(levels(dataset.df$PID), size = length(levels(dataset.df$PID)))

# SVM =================================================
library(e1071)
svm.outcome <- data.frame(matrix(nrow = 0, ncol = 2))
for(test.ppt in sample.ppts) {
     test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
     training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
     model <- svm(data = training.samples, class.sedentary ~ .)
     predicted <- predict(model, test.samples[, -ncol(test.samples)])
     svm.outcome <- rbind(svm.outcome,
                          data.frame(actual = as.character(test.samples$class.sedentary), predicted = as.character(predicted)))
}

confusion.matrix <- table(svm.outcome)
paste("SVM: Accuracy (", round((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix) * 100, digits = 2), ") & ",
      "Sensitivity (", round((confusion.matrix[2, 2]) / sum(confusion.matrix[, 2]), digits = 2), ") & ",
      "Specificity (", round((confusion.matrix[1, 1]) / sum(confusion.matrix[, 1]), digits = 2), ")", sep = "")

# Naive Bayes ========================================
naiveBayes.outcome <- data.frame(matrix(nrow = 0, ncol = 2))
for(test.ppt in sample.ppts) {
     test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
     training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
     model <- naiveBayes(data = training.samples, class.sedentary ~ .)
     predicted <- predict(model, test.samples[, -ncol(test.samples)])
     naiveBayes.outcome <- rbind(naiveBayes.outcome,
                          data.frame(actual = as.character(test.samples$class.sedentary), predicted = as.character(predicted)))
}

confusion.matrix <- table(naiveBayes.outcome)
paste("Naive Bayes: Accuracy (", round((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix) * 100, digits = 2), ") & ",
      "Sensitivity (", round((confusion.matrix[2, 2]) / sum(confusion.matrix[, 2]), digits = 2), ") & ",
      "Specificity (", round((confusion.matrix[1, 1]) / sum(confusion.matrix[, 1]), digits = 2), ")", sep = "")



# Decision Tree ======================================
library(rpart)
decisionTree.outcome <- data.frame(matrix(nrow = 0, ncol = 2))
for(test.ppt in sample.ppts) {
     test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
     training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
     model <- rpart(data = training.samples, class.sedentary ~ ., control = rpart.control(minsplit = 10, minbucket = 3), method = "class")
     predicted <- predict(model, test.samples[, -ncol(test.samples)], type = "class")
     decisionTree.outcome <- rbind(decisionTree.outcome,
                          data.frame(actual = as.character(test.samples$class.sedentary), predicted = as.character(predicted)))
}
confusion.matrix <- table(decisionTree.outcome)
paste("Decision Tree: Accuracy (", round((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix) * 100, digits = 2), ") & ",
      "Sensitivity (", round((confusion.matrix[2, 2]) / sum(confusion.matrix[, 2]), digits = 2), ") & ",
      "Specificity (", round((confusion.matrix[1, 1]) / sum(confusion.matrix[, 1]), digits = 2), ")", sep = "")


# Random Forest =========================================
library(randomForest)
set.seed(5855)
rf.model <- randomForest(data = dataset.df, class.sedentary ~ ., ntree = 10000, mtry = 2)
confusion.matrix <- rf.model$confusion[1:2, 1:2]
paste("Random Forest: Accuracy (", round((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix) * 100, digits = 2), ") & ",
      "Sensitivity (", round((confusion.matrix[2, 2]) / sum(confusion.matrix[, 2]), digits = 2), ") & ",
      "Specificity (", round((confusion.matrix[1, 1]) / sum(confusion.matrix[, 1]), digits = 2), ")", sep = "")




