# Constructing Staudenmayer Features for Wrist Accelerometer

source("f01_ml_features.R")

# This function converts a time string to its military (24hr) format.
# Examples:
#          a) 2:14:30  -->  14:14:30
#          b) 8:25:00  -->  08:25:00
convertToMilitaryTime <- function(timeStr) {
     if(is.na(timeStr)){
          return(NA)
     }
     tokens <- unlist(strsplit(timeStr, ":"))
     hour <- as.numeric(as.character(tokens[1]))
     if(hour <= 7) {
          hour <- hour + 12
     }
     hour <- as.character(hour)
     if(nchar(hour) < 2) {
          hour <- paste("0", hour, sep = "")
     }
     return(paste(as.character(hour), tokens[2], tokens[3], sep = ":"))
}


# Receives two arguments:
#     1. dataFolder: the address where participants' data are located.
#     2. participantID: participant unique identifier
#
# Finds wrist accelerometer files for the given participant and returns a list:
#     1. ppt.v1.df: a data.table consisting of V1 wrist accelerometer data
#     2. ppt.v2.df: a data.table consisting of V2 wrist accelerometer data
#     3. ppt.v3.df: a data.table consisting of V3 wrist accelerometer data
#     4. ppt.v4.df: a data.table consisting of V4 wrist accelerometer data
#     5. taskTimes.df: a data.frame containing tasktimes info: start and end times of each task
read.participant.files <- function(dataFolder, participantID) {
     require(data.table)
     
     # Locating participant's folder
     participantFolder <- dir(path = dataFolder, pattern = paste("^", participantID, ".*$", sep = ""))
     if(length(participantFolder) == 0) {
          stop("Participant Data are not found.") # if for a participant we do not have a data folder, we should not continue.
     }
     participantFolder <- paste(dataFolder, participantFolder, "/", sep = "")
     
     # Reading task times - logs which contain where a task started/ended and it was during which visit.
     taskTimesFileName <- dir(path = participantFolder, pattern = "^tasktimes.*$")
     if(length(taskTimesFileName) == 0) {
          stop("Participant has not task times file.") # If a participant does not have such log file, then there is no point considering him/her.
     }
     taskTimes.df <- read.csv(file = paste(participantFolder, taskTimesFileName, sep = ""), header = F, colClasses = rep("character", 6))
     # In the following line, a few preprocessing steps are required so we can easily read and use the data provided in taskTimes file.
     colnames(taskTimes.df) <- c("task", "cosmed.start", "cosmed.end", "phone.start", "phone.end", "visit.date")
     taskTimes.df$start <- sapply(taskTimes.df$phone.start, FUN = convertToMilitaryTime)
     taskTimes.df$end <- sapply(taskTimes.df$phone.end, FUN = convertToMilitaryTime)
     taskTimes.df <- taskTimes.df[, -c(2:5)]
     taskTimes.df$visit <- sapply(taskTimes.df$visit.date, FUN = function(x) {
          if(is.na(x)) {
               return(NA)
          }
          tokens <- unlist(strsplit(x, split = ":"))
          tokens[1]
     })
     taskTimes.df <- taskTimes.df[complete.cases(taskTimes.df), ]
     message("Tasktimes file found and read.")
     
     # Reading wrist accelerometer file for visit V1
     ppt.v1.df <- NA
     visitFileName <- dir(path = paste(participantFolder, "Wrist/", sep = ""), pattern = "^.*V1-.*$")
     if(length(visitFileName) == 0) {
          warning("Participant does not have Wrist data for V1 visit.")
     } else {
          ppt.v1.df <- fread(input = paste(participantFolder, "Wrist/", visitFileName, sep = "")) # similar to read.csv() function.
          colnames(ppt.v1.df) <- c("Timestamp", "X", "Y", "Z") # Renaming columns name for convenicence.
          
          ppt.v1.df$temp <- 1:nrow(ppt.v1.df)
          ppt.v1.df$Timestamp <- as.character(ppt.v1.df$Timestamp)
          ppt.v1.df[, VM := sqrt(X^2 + Y^2 + Z^2)] # calculating Vector Magnitude and adding a column 'VM' to our dataset
          # We also need to construct a timeOnly column for a more efficient search. See the three lines below.
          ppt.v1.df[, timeOnly := as.character(unlist(strsplit(Timestamp, split = " "))[2]), by = temp]
          ppt.v1.df[, timeOnly := as.character(unlist(strsplit(timeOnly, split = "\\."))[1]), by = temp]
          ppt.v1.df[, temp := NULL] 
     }
     message("V1 data loaded.")
     
     # Similar to reading wrist accelerometer file for visit 1.
     ppt.v2.df <- NA
     visitFileName <- dir(path = paste(participantFolder, "Wrist/", sep = ""), pattern = "^.*V2-.*$")
     if(length(visitFileName) == 0) {
          warning("Participant does not have Wrist data for V2 visit.")
     } else {
          ppt.v2.df <- fread(input = paste(participantFolder, "Wrist/", visitFileName, sep = ""))
          colnames(ppt.v2.df) <- c("Timestamp", "X", "Y", "Z")
          
          ppt.v2.df$temp <- 1:nrow(ppt.v2.df)
          ppt.v2.df$Timestamp <- as.character(ppt.v2.df$Timestamp)
          ppt.v2.df[, VM := sqrt(X^2 + Y^2 + Z^2)]
          ppt.v2.df[, timeOnly := as.character(unlist(strsplit(Timestamp, split = " "))[2]), by = temp]
          ppt.v2.df[, timeOnly := as.character(unlist(strsplit(timeOnly, split = "\\."))[1]), by = temp]
          ppt.v2.df[, temp := NULL]
     }
     message("V2 data loaded.")
     
     # Similar to reading wrist accelerometer file for visit 1.
     ppt.v3.df <- NA
     visitFileName <- dir(path = paste(participantFolder, "Wrist/", sep = ""), pattern = "^.*V3-.*$")
     if(length(visitFileName) == 0) {
          warning("Participant does not have Wrist data for V3 visit.")
     } else {
          ppt.v3.df <- fread(input = paste(participantFolder, "Wrist/", visitFileName, sep = ""))
          colnames(ppt.v3.df) <- c("Timestamp", "X", "Y", "Z")
          
          ppt.v3.df$temp <- 1:nrow(ppt.v3.df)
          ppt.v3.df$Timestamp <- as.character(ppt.v3.df$Timestamp)
          ppt.v3.df[, VM := sqrt(X^2 + Y^2 + Z^2)]
          ppt.v3.df[, timeOnly := as.character(unlist(strsplit(Timestamp, split = " "))[2]), by = temp]
          ppt.v3.df[, timeOnly := as.character(unlist(strsplit(timeOnly, split = "\\."))[1]), by = temp]
          ppt.v3.df[, temp := NULL]
     }
     message("V3 data loaded.")
     
     # Similar to reading wrist accelerometer file for visit 1.
     ppt.v4.df <- NA
     visitFileName <- dir(path = paste(participantFolder, "Wrist/", sep = ""), pattern = "^.*V4-.*$")
     if(length(visitFileName) == 0) {
          warning("Participant does not have Wrist data for V4 visit.")
     } else {
          ppt.v4.df <- fread(input = paste(participantFolder, "Wrist/", visitFileName, sep = ""))
          colnames(ppt.v4.df) <- c("Timestamp", "X", "Y", "Z")
          
          ppt.v4.df$temp <- 1:nrow(ppt.v4.df)
          ppt.v4.df$Timestamp <- as.character(ppt.v4.df$Timestamp)
          ppt.v4.df[, VM := sqrt(X^2 + Y^2 + Z^2)]
          ppt.v4.df[, timeOnly := as.character(unlist(strsplit(Timestamp, split = " "))[2]), by = temp]
          ppt.v4.df[, timeOnly := as.character(unlist(strsplit(timeOnly, split = "\\."))[1]), by = temp]
          ppt.v4.df[, temp := NULL]
     }
     message("V4 data loaded.")
     
     result <- list(ppt.v1.df, ppt.v2.df, ppt.v3.df, ppt.v4.df, taskTimes.df)
     result
}

# Input:
#     1. participantID: participant unique identifier
#     2. cosmed.df: (data.frame) containing all cosmed information for every participant-task row
#     3-7. ppt.vX.df: (data.table) containing wrist accelerometer data for VX
#     8. taskTimes.df: (data.frame) containing tasktimes data: start and end times of each task for the given participant
#     9. epoch.length: (integer) indicates each epoch length (in 10 milliseconds) for which we construct features
#
# Output:
#     1. returns a data.frame which contains feature vectors for each <Participant, Task, Epoch>.
ML.features.oneParticipant <- function(participantID, cosmed.df, ppt.v1.df, ppt.v2.df, ppt.v3.df, ppt.v4.df, taskTimes.df, epoch.length = (15 * 100)) {
     message(paste("Constructing features for", participantID, " started."))

     result <- data.frame(matrix(nrow = 0, ncol = 12))
     
     # Selecting the part of cosmed.df dataframe which is related to the current participant.
     ppt.cosmed.df <- cosmed.df[cosmed.df$participant_acrostic == participantID, ]
     
     # Constructing features #
     for(i in 1:nrow(taskTimes.df)){
          taskTimes.ppt.df <- taskTimes.df[i, ]
          taskDataFrame <- ppt.v1.df
          Task <- taskTimes.ppt.df$task[1]
          message(paste(participantID, Task, sep = " -- "))
          if(taskTimes.ppt.df$visit[1] == 'V2') {
               taskDataFrame <- ppt.v2.df
          } else if(taskTimes.ppt.df$visit[1] == 'V3') {
               taskDataFrame <- ppt.v3.df
          } else if(taskTimes.ppt.df$visit[1] == 'V4') {
               taskDataFrame <- ppt.v4.df
          }
          process.status <- TRUE
          start.idx <- which(taskDataFrame$timeOnly == taskTimes.ppt.df$start[1])
          if(length(start.idx) == 0) {
               warning(paste("No start time found for participant (", participantID, ") and task (", Task, ")", sep = ""))
               process.status <- FALSE
               next
          } else {
               start.idx <- min(start.idx)
          }
          end.idx <- which(taskDataFrame$timeOnly == taskTimes.ppt.df$end[1])
          if(length(end.idx) == 0) {
               warning(paste("No end time found for participant (", participantID, ") and task (", Task, ")", sep = ""))
               process.status <- FALSE
               next
          } else {
               end.idx <- max(end.idx)
          }
          if(process.status) {
               METs <- NA
               MET.idx <- which(ppt.cosmed.df$task == Task)
               if(length(MET.idx) > 0) {
                    METs <- ppt.cosmed.df$METs[MET.idx]
               }
               
               feature.df <- ml.featureConstruction(PID = participantID, Task = taskTimes.ppt.df$task[1], METs, taskDataFrame, start.idx, end.idx, epoch.length)
               if(nrow(feature.df) > 0) {
                    result <- rbind(result, feature.df)
               }
          }
     }
     result
}


