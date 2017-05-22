rm(list = ls())
setwd("~/Workspaces/R workspace/Activity Recognition/pkg01_Data_Cleaning/")

# Check 'participant_age.csv' file to see how our participant IDs look like.
participantID <- "TALI002" # This is the first participant in the file.

# This is the address of the folder in your computer where participants' accelerometer files are located.
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/"

# Window length: indicating each atom has how many seconds of data
window.length <- 6 * 10
dataFolder_windowLength <- "Six-second chunks/"

# The main script --------------------------------------------------------------------------------
# construct features for each participant and saves into a Rdata file

source("f02_ML_features_one_participant.R")
source("f04_bow.R")
rm(convert.fft, df, fpdf, mangle, ml.featureConstruction, ml.features.oneEpoch, ML.features.oneParticipant, mvm, p625, sdangle, sdvm)

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
# Downsample to 10 Hz
options(warn = -1)
for(i in 1:4) {
     if(!is.na(data.files[[i]])) {
          sampling.rate <- find.samplingRate(data.files[[i]])
          data.files[[i]] <- downSampleToTenHz(data.files[[i]], sampling.rate)
     }
}
rm(i, sampling.rate, find.samplingRate, downSampleToTenHz, read.participant.files)
options(warn = 0)

# Checking if tasks have overlapping times 
taskTimes.df <- data.files[[5]]
print.overlapping.tasks(taskTimes.df)

# Constructing bow chunks
bowChunks.df <- bow.timeSeriesChunks.oneParticipant(participantID = participantID,
                                         cosmed.df = cosmed.df,
                                         ppt.v1.df = data.files[[1]], ppt.v2.df = data.files[[2]], ppt.v3.df = data.files[[3]], ppt.v4.df = data.files[[4]],
                                         taskTimes.df = data.files[[5]],
                                         window.length = window.length)

save(bowChunks.df, file = paste(dataFolder, "BOW_Files/", dataFolder_windowLength, participantID, "_wrist_BoW.Rdata", sep = ""))


