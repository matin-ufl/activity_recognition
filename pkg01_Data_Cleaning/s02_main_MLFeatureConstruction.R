# Parameters for constructing features -----------------------------------------------------------

# Check 'participant_age.csv' file to see how our participant IDs look like.
participantID <- "HEAH055" # This is the first participant in the file.

# This is the address of the folder in your computer where participants' accelerometer files are located.
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/"


# The main script --------------------------------------------------------------------------------
# construct features for each participant and saves into a Rdata file

setwd("~/Workspaces/R workspace/Activity Recognition/pkg01_Data_Cleaning/")
source("f02_ML_features_one_participant.R")


# To obtain MET scores (energy expenditure) for each task, we have to read 'cosmed_mets.csv' file.
# However, we do not need to read this file for every participant since it contains the information for all participant-tasks.
# We better read it once into a dataframe and pass it to our functions.
cosmed.df <- read.csv(file = paste(dataFolder, "../cosmed_mets.csv", sep = ""))
cosmed.df$activity <- as.character(cosmed.df$activity)

# The task names are different in tasktimes files and cosmed. Therefore, we need to make them equal for our processing.
tasknames_mapping.df <- read.csv(file = paste(dataFolder, "../taskNames_mapping.csv", sep = ""), colClasses = rep("character", 2))
cosmed.df$task <- sapply(cosmed.df$activity, FUN = function(x) {
     result <- which(tasknames_mapping.df$Cosmed_Task == x)
     if(length(result) == 0) {
          return(NA)
     }
     res <- tasknames_mapping.df$Task[result]
     res
})

# Loading visit files 
data.files <- read.participant.files(dataFolder, participantID)

# Constructing features
feature.df <- ML.features.oneParticipant(participantID = participantID,
                                cosmed.df = cosmed.df,
                                ppt.v1.df = data.files[[1]], ppt.v2.df = data.files[[2]], ppt.v3.df = data.files[[3]], ppt.v4.df = data.files[[4]],
                                taskTimes.df = data.files[[5]],
                                epoch.length = (15 * 100))

save(feature.df, file = paste(dataFolder, participantID, "_wrist_features.Rdata", sep = ""))
