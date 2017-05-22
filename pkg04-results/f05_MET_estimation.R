# MET estimation functions.
# All the methods are based on Leave-one-out cross-validation
# the given.df dataset should have a 'MET' column in the end,
#     and the rest of the variables in the begining.


loocv.SVM <- function(given.df) {
     require(e1071)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$MET <- as.numeric(given.df[, ncol(given.df)]) # MET value should be numeric 
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          model <- svm(data = training.samples, MET ~ .)
          predicted <- predict(model, test.sample[, -ncol(test.sample)])
          outcome <- rbind(outcome,
                           data.frame(actual = as.numeric(test.sample$MET), predicted = as.numeric(predicted)))
     }
     outcome
}

loocv.DecisionTree <- function(given.df, minsplit = 10, minbucket = 3) {
     require(rpart)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$MET <- as.numeric(given.df[, ncol(given.df)]) # MET value should be numeric 
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          model <- rpart(data = training.samples, MET ~ ., control = rpart.control(minsplit = minsplit, minbucket = minbucket))
          predicted <- predict(model, test.sample[, -ncol(test.sample)], method = "")
          outcome <- rbind(outcome,
                           data.frame(actual = as.numeric(test.sample$MET), predicted = as.numeric(predicted)))
     }
     outcome
}

loocv.RandomForest <- function(given.df, ntree = 10000, mtry = 2) {
     require(randomForest)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$MET <- as.numeric(given.df[, ncol(given.df)]) # MET value should be numeric 
     
     outcome <- data.frame(matrix(nrow = 0, ncol = 2))
     set.seed(5855)
     test.idx <- sample.int(n = nrow(dataset.df), replace = F)
     for(i in test.idx) {
          test.sample <- dataset.df[i, ]
          training.samples <- dataset.df[-i, ]
          set.seed(5855)
          rf.model <- randomForest(data = training.samples, MET ~ ., ntree = ntree, mtry = mtry)
          predicted <- as.character(predict(rf.model, test.sample[, -ncol(test.sample)]))
          outcome <- rbind(outcome, data.frame(actual = as.numeric(test.sample$MET), predicted = as.numeric(predicted)))
     }
     outcome
}

# This function takes the classifier's outcome and prints the accuracy, sensitivity, specificity, etc.
# Input:
#          1. classifier.outcome: a data.frame with 'actual' & 'predicted' columns.
print.regressionStatistics <- function(estimate.outcome, method.name = 'Estimation') {
     mse <- mean((estimate.outcome$actual - estimate.outcome$predicted)^2)
     rMSE <- round(sqrt(mse), digits = 4)
     linModel <- lm(actual~predicted, estimate.outcome)
     linModel2 <- lm(predicted~actual, estimate.outcome)
     s <- summary(linModel)
     r.sq <- round(s$adj.r.squared, digits = 4)
     s2 <- summary(linModel2)
     r.sq2 <- round(s2$adj.r.squared, digits = 4)
     print(paste(method.name, " & rMSE (", rMSE, ") & R2 (", r.sq, ") or (", r.sq2, ")", sep = ""))
}
