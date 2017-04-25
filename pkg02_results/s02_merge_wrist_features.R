###############################################################
# Reads all the 15-sec epoch variables for each participant-  #
# task, and creates a feature vector for it.                  #
# ___________________________________________________________ #
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)                #
###############################################################

setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")

# Loading 15-second epoch features
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/ML_Features/"
l <- dir(dataFolder, pattern = "^.*.Rdata$")
wrist.df <- data.frame(matrix(nrow = 0, ncol = 12))
for(filename in l) {
     load(paste(dataFolder, filename, sep = ""))
     wrist.df <- rbind(wrist.df, feature.df)
}
rm(filename, feature.df, l)

# Collapsing 15-sec epoch features, into one feature vector for every ppt-task
activityRecognition.df <- data.frame(matrix(nrow = 0, ncol = 12))
for(ppt in levels(as.factor(wrist.df$PID))) {
     message(paste(as.character(ppt)))
     ppt.df <- wrist.df[wrist.df$PID == ppt, ]
     for(task in levels(as.factor(ppt.df$Task))) {
          ppt.task.df <- ppt.df[ppt.df$Task == task, ]
          if(nrow(ppt.task.df) > 0) {
               temp <- data.frame(PID = as.character(ppt),
                                  Task = as.character(task),
                                  startTime = as.character(ppt.task.df$start.time[1]),
                                  endTime = as.character(ppt.task.df$end.time[nrow(ppt.task.df)]),
                                  MVM = mean(ppt.task.df$MVM, na.rm = T),
                                  SDVM = mean(ppt.task.df$SDVM, na.rm = T),
                                  MANGLE = mean(ppt.task.df$MANGLE, na.rm = T),
                                  SDANGLE = mean(ppt.task.df$SDANGLE, na.rm = T),
                                  P625 = mean(ppt.task.df$P625, na.rm = T),
                                  DF = mean(ppt.task.df$DF, na.rm = T),
                                  FPDF = mean(ppt.task.df$FPDF, na.rm = T),
                                  MET = ppt.task.df$MET[1])
               activityRecognition.df <- rbind(activityRecognition.df, temp)
          }
     }
}
rm(ppt, task, temp, ppt.task.df, ppt.df)

a <- levels(activityRecognition.df$Task)
activityRecognition.df <- activityRecognition.df[activityRecognition.df$Task %in% a[1:33], ]
activityRecognition.df$Task <- as.character(activityRecognition.df$Task)
activityRecognition.df$Task <- as.factor(activityRecognition.df$Task)
activityRecognition.df$class.locomotion <- "No"
activityRecognition.df$class.locomotion[activityRecognition.df$Task %in% c("LEISURE WALK", "RAPID WALK", "WALKING AT RPE 1", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT")] <- "Yes"
activityRecognition.df$class.sedentary <- "No"
activityRecognition.df$class.sedentary[activityRecognition.df$Task %in% c("COMPUTER WORK", "TV WATCHING", "STANDING STILL")] <- "Yes"
rm(a)

save(activityRecognition.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/wrist_features_042417.Rdata")

# Getting a summary of the tasks (for a table in the manuscript)
for(task in levels(as.factor(activityRecognition.df$Task))) {
     task.df <- activityRecognition.df[activityRecognition.df$Task == task, ]
     if(nrow(task.df) > 0) {
          print(paste(task, " & ",
                      "METS: ", round(mean(task.df$MET, na.rm = T), digits = 2), " (", round(sd(task.df$MET, na.rm = T), digits = 2), ") & ",
                      "MVM: ", round(mean(task.df$MVM, na.rm = T), digits = 2), " (", round(sd(task.df$MVM, na.rm = T), digits = 2), ") & ",
                      "SDVM: ", round(mean(task.df$SDVM, na.rm = T), digits = 2), " (", round(sd(task.df$SDVM, na.rm = T), digits = 2), ") & ",
                      "MANGLE: ", round(mean(task.df$MANGLE, na.rm = T), digits = 2), " (", round(sd(task.df$MANGLE, na.rm = T), digits = 2), ") & ",
                      "SDANGLE: ", round(mean(task.df$SDANGLE, na.rm = T), digits = 2), " (", round(sd(task.df$SDANGLE, na.rm = T), digits = 2), ") & ",
                      "P625: ", round(mean(task.df$P625, na.rm = T), digits = 2), " (", round(sd(task.df$P625, na.rm = T), digits = 2), ") & ",
                      "DF: ", round(mean(task.df$DF, na.rm = T), digits = 2), " (", round(sd(task.df$DF, na.rm = T), digits = 2), ") & ",
                      "FPDF: ", round(mean(task.df$FPDF, na.rm = T), digits = 2), " (", round(sd(task.df$FPDF, na.rm = T), digits = 2), ") \\",
                      sep = ""))
     }
}


b$Task <- as.character(b$Task)
