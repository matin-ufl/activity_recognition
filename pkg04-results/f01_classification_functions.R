# Classification functions for sedentary task detection
# All of the functions in this file, takes a dataset with
#     1. PID and Task first.
#     2. Variables...
#     3. class.label last.


loocv.SVM <- function(given.df, outcome.levels = c("Yes", "No")) {
     require(e1071)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], levels = outcome.levels) # class.label should be a factor
     dataset.df <- dataset.df[, -c(1:2)] # excluding PID and Task columns
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          model <- svm(data = training.samples, class.label ~ .)
          predicted <- predict(model, test.sample[, -ncol(test.sample)])
          outcome <- rbind(outcome,
                           data.frame(actual = as.character(test.sample$class.label), predicted = as.character(predicted)))
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}

# The given dataset has all the variables first, and in the end, it has the target variable.
loocv.LogisticRegression <- function(given.df, lambda = 0.01, outcome.levels = c("Yes", "No")) {
     require(glmnet)
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], levels = outcome.levels)
     dataset.df <- dataset.df[, -c(1:2)] # excluding PID and Task columns

     family <- "binomial"
     if(length(levels(dataset.df$class.label)) > 2) {
          family <- "multinomial"
     }
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          x <- as.matrix(training.samples[, -ncol(training.samples)])
          y <- training.samples$class.label
          model <- glmnet(x = x, y = y, family = family, lambda = lambda)
          predicted.response <- predict(model, as.matrix(test.sample[, -ncol(dataset.df)]), type = 'class')
          outcome <- rbind(outcome, data.frame(actual = as.character(test.sample$class.label), predicted = as.character(predicted.response)))
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}


loocv.DecisionTree <- function(given.df, minsplit = 10, minbucket = 3, outcome.levels = c("Yes", "No")) {
     require(rpart)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], levels = outcome.levels)
     dataset.df <- dataset.df[, -c(1:2)] # excluding PID and Task columns
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          model <- rpart(data = training.samples, class.label ~ ., control = rpart.control(minsplit = 10, minbucket = 3), method = "class")
          predicted <- predict(model, test.sample[, -ncol(test.sample)], type = "class")
          outcome <- rbind(outcome,
                           data.frame(actual = as.character(test.sample$class.label), predicted = as.character(predicted)))
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}

loocv.RandomForest <- function(given.df, ntree = 10000, mtry = 2, outcome.levels = c("Yes", "No")) {
     require(randomForest)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], outcome.levels)
     dataset.df <- dataset.df[, -c(1:2)] # excluding PID and Task columns
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          set.seed(5855)
          rf.model <- randomForest(data = training.samples, class.label ~ ., ntree = ntree, mtry = mtry)
          predicted <- as.character(predict(rf.model, test.sample[, -ncol(test.sample)]))
          outcome <- rbind(outcome, data.frame(actual = as.character(test.sample$class.label), predicted = predicted))
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}

# This function takes the classifier's outcome and prints the accuracy, sensitivity, specificity, etc.
# Input:
#          1. classifier.outcome: a data.frame with 'actual' & 'predicted' columns.
print.classificationStatistics <- function(classifier.outcome, classifier.name = 'classifier', outcome.levels = c("Yes", "No")) {
     classifier.outcome$actual <- factor(classifier.outcome$actual, levels = outcome.levels)
     classifier.outcome$predicted <- factor(classifier.outcome$predicted, levels = outcome.levels)
     
     confusion.matrix <- table(classifier.outcome)
     if(ncol(confusion.matrix) < 2) {
          confusion.matrix <- cbind(confusion.matrix, matrix(c(0, 0), ncol = 1))
     }
     if(nrow(confusion.matrix) < 2) {
          confusion.matrix <- rbind(confusion.matrix, matrix(c(0, 0), nrow = 1))
     }
     accuracy <- ((confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix)) * 100 # (TP + TN) / (TP + FP + TN + FN)
     sensitivity <- confusion.matrix[1, 1] / sum(confusion.matrix[1, ]) # TP / (TP + FN)
     precision <- confusion.matrix[1, 1] / sum(confusion.matrix[, 1]) # TP / (TP + FP)
     f1.score <- (2 * confusion.matrix[1, 1]) / ((2 * confusion.matrix[1, 1]) + confusion.matrix[2, 1] + confusion.matrix[1, 2]) # 2 TP / (2 TP + FP + FN)
     paste(classifier.name, ": Accuracy (", round(accuracy, digits = 2), ") & ",
           "Sensitivity (", round(sensitivity, digits = 2), ") & ",
           "Precision (", round(precision, digits = 2), ") & ",
           "F1-score (", round(f1.score, digits = 2), ")", sep = "")
}
