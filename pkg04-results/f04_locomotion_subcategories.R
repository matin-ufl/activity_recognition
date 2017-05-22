# Classification functions for locomotion-subcategory detection
# All of the functions in this file, takes a dataset with
#     1. PID and Task first.
#     2. Variables...
#     3. class.label last.

loocv.RandomForest <- function(given.df, ntree = 10000, mtry = 2, outcome.levels = c("LEISURE WALK", "RAPID WALK", "WALKING AT RPE 1", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT")) {
     require(randomForest)
     # Making sure the given dataset has the right column names
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(as.character(given.df[, ncol(given.df)]), levels = outcome.levels) # class.label should be a factor
     
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
print.subCategory.classificationStatistics <- function(classifier.outcome, classifier.name = 'classifier', outcome.levels = c("LEISURE WALK", "WALKING AT RPE 1", "RAPID WALK", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT")) {
     classifier.outcome$actual <- factor(classifier.outcome$actual, levels = outcome.levels)
     classifier.outcome$predicted <- factor(classifier.outcome$predicted, levels = outcome.levels)
     
     confusion.matrix <- table(classifier.outcome)
     while(ncol(confusion.matrix) < nrow(confusion.matrix)) {
          confusion.matrix <- cbind(confusion.matrix, matrix(rep(0, nrow(confusion.matrix)), ncol = 1))
     }
     while(nrow(confusion.matrix) < ncol(confusion.matrix)) {
          confusion.matrix <- rbind(confusion.matrix, matrix(rep(0, ncol(confusion.matrix)), nrow = 1))
     }
     diagSum <- 0
     for(i in 1:nrow(confusion.matrix)) {
          diagSum <- diagSum + confusion.matrix[i, i]
     }
     accuracy <- (diagSum / sum(confusion.matrix)) * 100 # (TP + TN) / (TP + FP + TN + FN)
     print(paste(classifier.name, ": ", round(accuracy, digits = 2), " accuracy.", sep = ""))
}
