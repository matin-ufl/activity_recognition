################################################
# Staudenmayer's paper:
#     1. Decision tree
#     2. Linear Regression
# _____________________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
################################################

Staudenmayer.sedentary.decisionTree <- function(given.df, outcome.levels = c("Yes", "No")) {
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], levels = outcome.levels)
     
     outcome <- data.frame(actual = dataset.df$class.label, predicted = "No")
     outcome$predicted <- as.character(outcome$predicted)
     for(i in 1:nrow(dataset.df)) {
          if(dataset.df$SDVM[i] <= 0.098) {
               if(dataset.df$P625[i] <= 0.138) {
                    outcome$predicted[i] <- "Yes"
               }
          }
          if(dataset.df$SDVM[i] <= 0.062) {
               if(dataset.df$P625[i] > 0.138) {
                    outcome$predicted[i] <- "Yes"
               }
          }
          if(dataset.df$SDVM[i] > 0.098) {
               if(dataset.df$SDVM[i] <= 0.148) {
                    if(dataset.df$P625[i] <= 0.118) {
                         outcome$predicted[i] <- "Yes"
                    }
               }
          }
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}


Staudenmayer.locomotion.decisionTree <- function(given.df, outcome.levels = c("Yes", "No")) {
     dataset.df <- given.df[, -ncol(given.df)]
     dataset.df$class.label <- factor(given.df[, ncol(given.df)], levels = outcome.levels)
     
     outcome <- data.frame(actual = dataset.df$class.label, predicted = "No")
     outcome$predicted <- as.character(outcome$predicted)
     for(i in 1:nrow(dataset.df)) {
          if(dataset.df$FPDF[i] > 0.020) {
               if(dataset.df$MANGLE[i] <= -53) {
                    outcome$predicted[i] <- "Yes"
               }
          }
          if(dataset.df$FPDF[i] > 0.039) {
               if(dataset.df$MANGLE[i] <= -62) {
                    outcome$predicted[i] <- "Yes"
               }
          }
          if(dataset.df$FPDF[i] > 0.06) {
               if(dataset.df$MANGLE[i] > -62) {
                    outcome$predicted[i] <- "Yes"
               }
          }
     }
     outcome$actual <- factor(outcome$actual, levels = outcome.levels)
     outcome$predicted <- factor(outcome$predicted, levels = outcome.levels)
     outcome
}

Staudenmayer.activityIntensity.decisionTree <- function(dataset.df) {
     outcome <- data.frame(actual = dataset.df$class.activityIntensity, predicted = "MODERATE")
     outcome$predicted <- as.character(outcome$predicted)
     for(i in 1:nrow(dataset.df)) {
          if(dataset.df$SDVM[i] <= 0.26) {
               if(dataset.df$MANGLE[i] > -52) {
                    outcome$predicted[i] <- "LIGHT"
               }
          }
          if(dataset.df$SDVM[i] > 0.26) {
               if(dataset.df$SDVM[i] <= 0.79) {
                    if(dataset.df$MANGLE[i] <= -53) {
                         outcome$predicted[i] <- "VIGOROUS"
                    }
               }
          }
          if(dataset.df$SDVM[i] > 0.79) {
               outcome$predicted[i] <- "VIGOROUS"
          }
     }
     outcome
}

Staudenmayer.MET.linearRegression <- function(dataset.df) {
     outcome <- data.frame(actual = dataset.df$MET,
                           predicted = (1.89378 + (5.50821 * dataset.df$SDVM) - (0.02705 * dataset.df$MANGLE))
                           )
     outcome$actual[outcome$actual < 1] <- 1
     outcome
}