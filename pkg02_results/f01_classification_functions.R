# Classification functions for activity recognition
# All of the functions in this file, takes a dataset with
#     1. PID first. PID is the participant unique identifier.
#     2. Variables...
#     3. class.label last.


mySVM <- function(dataset.df) {
     require(e1071)
     # Making sure the given dataset has the right column names
     colnames(dataset.df) <- c("PID", colnames(dataset.df)[2:(ncol(dataset.df) - 1)], "class.label")
     dataset.df$class.label <- as.factor(dataset.df$class.label)
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     
     for(test.ppt in levels(as.factor(dataset.df$PID))) {
          test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
          training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
          model <- svm(data = trainingSet, class.label ~ .)
          predicted <- predict(model, test.samples[, -ncol(testSample)])
          outcome <- rbind(outcome,
                           data.frame(actual = as.character(testSample$class.label), predicted = as.character(predicted)))
     }
     outcome
}

# The given dataset has all the variables first, and in the end, it has the target variable.
myLogisticRegression <- function(dataset.df, lambda = 0.01) {
     require(glmnet)
     colnames(dataset.df) <- c("PID", colnames(dataset.df)[2:(ncol(dataset.df) - 1)], "class.label")
     dataset.df$class.label <- as.factor(dataset.df$class.label)
     family <- "binomial"
     if(length(levels(dataset.df$class.label)) > 2) {
          family <- "multinomial"
     }
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     for(test.ppt in levels(as.factor(dataset.df$PID))) {
          test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
          training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
          x <- as.matrix(training.samples[, -ncol(training.samples)])
          y <- training.samples$class.label
          model <- glmnet(x = x, y = y, family = family, lambda = lambda)
          predicted.response <- predict(model, as.matrix(test.samples[, -ncol(dataset.df)]), type = 'class')
          outcome <- rbind(outcome, data.frame(actual = as.character(test.samples$class.label), predicted = as.character(predicted.response)))
     }
     
     outcome
}


myDecisionTree <- function(dataset.df, minsplit = 10, minbucket = 3) {
     # Making sure the given dataset has the right column names
     colnames(dataset.df) <- c("PID", colnames(dataset.df)[2:(ncol(dataset.df) - 1)], "class.label")
     require(rpart)
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     for(test.ppt in levels(as.factor(dataset.df$PID))) {
          test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
          training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
          model <- rpart(data = training.samples, class.label ~ ., control = rpart.control(minsplit = 10, minbucket = 3), method = "class")
          predicted <- predict(model, test.samples[, -ncol(test.samples)], type = "class")
          outcome <- rbind(outcome,
                           data.frame(actual = as.character(test.samples$class.label), predicted = as.character(predicted)))
     }
     outcome
}

myRandomForest <- function(dataset.df, ntree = 10000, mtry = 2, seed = 5855) {
     # Making sure the given dataset has the right column names
     colnames(dataset.df) <- c("PID", colnames(dataset.df)[2:(ncol(dataset.df) - 1)], "class.label")
     dataset.df$class.label <- as.factor(dataset.df$class.label)
     require(randomForest)
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     for(test.ppt in levels(as.factor(dataset.df$PID))) {
          test.samples <- dataset.df[dataset.df$PID == test.ppt, -1]
          training.samples <- dataset.df[dataset.df$PID != test.ppt, -1]
          rf.model <- randomForest(data = training.samples, class.label ~ ., ntree = ntree, mtry = mtry)
          predicted <- as.character(predict(rf.model, test.samples[, -ncol(test.samples)]))
          outcome <- rbind(outcome, data.frame(actual = as.character(test.samples$class.label), predicted = predicted))
     }
     outcome
}

# This function takes the classifier's outcome and prints the accuracy, sensitivity, specificity, etc.
# Input:
#          1. classifier.outcome: a data.frame with 'actual' & 'predicted' columns.
print.classificationStatistics <- function(classifier.outcome, classifier.name = 'classifier') {
     confusion.matrix <- table(classifier.outcome)
     if(ncol(confusion.matrix) < 2) {
          confusion.matrix <- cbind(confusion.matrix, matrix(c(0, 0), ncol = 1))
     }
     if(nrow(confusion.matrix) < 2) {
          confusion.matrix <- rbind(confusion.matrix, matrix(c(0, 0), nrow = 1))
     }
     accuracy <- ((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix)) * 100 # (TP + TN) / (TP + FP + TN + FN)
     sensitivity <- confusion.matrix[2, 2] / sum(confusion.matrix[2, ]) # TP / (TP + FN)
     precision <- confusion.matrix[2, 2] / sum(confusion.matrix[, 2]) # TP / (TP + FP)
     f1.score <- (2 * confusion.matrix[2, 2]) / ((2 * confusion.matrix[2, 2]) + confusion.matrix[1, 2] + confusion.matrix[2, 1]) # 2 TP / (2 TP + FP + FN)
     paste(classifier.name, ": Accuracy (", round(accuracy, digits = 2), ") & ",
           "Sensitivity (", round(sensitivity, digits = 2), ") & ",
           "Precision (", round(precision, digits = 2), ") & ",
           "F1-score (", round(f1.score, digits = 2), ")", sep = "")
}