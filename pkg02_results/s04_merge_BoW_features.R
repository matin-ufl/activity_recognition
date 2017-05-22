###############################################################
# Reads all the Bag-of-Wordsvariables for each participant-   #
# task, and creates a feature vector for it.                  #
# ___________________________________________________________ #
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)                #
###############################################################

setwd("~/Workspaces/R workspace/Activity Recognition/pkg02_results/")

# Loading 15-second epoch features
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/BOW_Files/"
l <- dir(dataFolder, pattern = "^.*.Rdata$")
wrist.df <- data.frame(matrix(nrow = 0, ncol = 12))
for(filename in l) {
     load(paste(dataFolder, filename, sep = ""))
     wrist.df <- rbind(wrist.df, bow.df)
}
rm(filename, bow.df, l)

# Fixing task title issues
activityRecognition.bow.df <- wrist.df
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Rapid Walking"] <- "RAPID WALK"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Leisure Walk"] <- "LEISURE WALK"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Light Gardening"] <- "LIGHT GARDENING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Yard Work"] <- "YARD WORK"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Digging"] <- "DIGGING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Prepare and Serve a Meal"] <- "PREPARE SERVE MEAL"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Straightening Up"] <- "STRAIGHTENING UP DUSTING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Washing Dishes"] <- "WASHING DISHES"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Unloading and Storing Dishes"] <- "UNLOADING STORING DISHES"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Walking- RPE1"] <- "WALKING AT RPE 1"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Personal Care"] <- "PERSONAL CARE"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Dressing"] <- "DRESSING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Walking-RPE5"] <- "WALKING AT RPE 5"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Sweeping"] <- "SWEEPING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Vacuuming"] <- "VACUUMING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Stair Descent"] <- "STAIR DESCENT"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Stair Ascent"] <- "STAIR ASCENT"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Trash Removal"] <- "TRASH REMOVAL"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Stretching and Yoga"] <- "STRETCHING YOGA"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Replacing Sheets on a Bed"] <- "REPLACING SHEETS ON A BED"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Mopping"] <- "MOPPING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Computer Work"] <- "COMPUTER WORK"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Laundry Washing"] <- "LAUNDRY WASHING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Light Home Maintenacne"] <- "LIGHT HOME MAINTENANCE"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Washing Windows"] <- "WASHING WINDOWS"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Shopping"] <- "SHOPPING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Ironing"] <- "IRONING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Heavy Lifting"] <- "HEAVY LIFTING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Strength Exercise-Chest Press"] <- "STRENGTH EXERCISE CHEST PRESS"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Strength Exercise- Leg curl"] <- "STRENGTH EXERCISE LEG CURL"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Strength Exercise- Leg extension"] <- "STRENGTH EXERCISE LEG EXTENSION"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "TV Watching"] <- "TV WATCHING"
activityRecognition.bow.df$Task[activityRecognition.bow.df$Task == "Standing Still"] <- "STANDING STILL"


a <- levels(activityRecognition.bow.df$Task)
activityRecognition.bow.df <- activityRecognition.bow.df[activityRecognition.bow.df$Task %in% a[1:33], ]
activityRecognition.bow.df$Task <- as.character(activityRecognition.bow.df$Task)
activityRecognition.bow.df$Task <- as.factor(activityRecognition.bow.df$Task)
activityRecognition.bow.df$class.locomotion <- "No"
activityRecognition.bow.df$class.locomotion[activityRecognition.bow.df$Task %in% c("LEISURE WALK", "RAPID WALK", "WALKING AT RPE 1", "WALKING AT RPE 5", "STAIR ASCENT", "STAIR DESCENT")] <- "Yes"
activityRecognition.bow.df$class.sedentary <- "No"
activityRecognition.bow.df$class.sedentary[activityRecognition.bow.df$Task %in% c("COMPUTER WORK", "TV WATCHING", "STANDING STILL")] <- "Yes"
rm(a)


save(activityRecognition.bow.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/bow_features_042517.Rdata")

# Normalizing the dataset
bow.df <- activityRecognition.bow.df
for(i in 7:14) {
     bow.df[, i] <- scale(bow.df[, i])
}
rm(i)

save(bow.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/bow_normal_042517.Rdata")

# Getting a summary of the tasks (for a table in the manuscript)
for(task in levels(as.factor(activityRecognition.bow.df$Task))) {
     task.df <- activityRecognition.bow.df[activityRecognition.bow.df$Task == task, ]
     if(nrow(task.df) > 0) {
          print(paste(task, " & ",
                      "METS: ", round(mean(task.df$MET, na.rm = T), digits = 2), " (", round(sd(task.df$MET, na.rm = T), digits = 2), ") & ",
                      "TFIDF-A: ", round(mean(task.df$tfidf.A, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.A, na.rm = T), digits = 2), ") & ",
                      "TFIDF-B: ", round(mean(task.df$tfidf.B, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.B, na.rm = T), digits = 2), ") & ",
                      "TFIDF-C: ", round(mean(task.df$tfidf.C, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.C, na.rm = T), digits = 2), ") & ",
                      "TFIDF-D: ", round(mean(task.df$tfidf.D, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.D, na.rm = T), digits = 2), ") & ",
                      "TFIDF-E: ", round(mean(task.df$tfidf.E, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.E, na.rm = T), digits = 2), ") & ",
                      "TFIDF-F: ", round(mean(task.df$tfidf.G, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.G, na.rm = T), digits = 2), ") & ",
                      "TFIDF-G: ", round(mean(task.df$tfidf.G, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.G, na.rm = T), digits = 2), ") & ",
                      "TFIDF-H: ", round(mean(task.df$tfidf.H, na.rm = T), digits = 2), " (", round(sd(task.df$tfidf.H, na.rm = T), digits = 2), ") \\",
                      sep = ""))
     }
}


