# Converts accelerometer data into a Bag of Words.
# 1. A threshold of 3 G (m/s2) is defined. Let's call it 'threshold'.
# 2. For every task, we define a logical array. Let's call it 'th_arr'.
#     2.1. the accelerometer data (VM) is checked for every second. In our dataset, every second has 100 samples.
#     2.2. if the accelerometer value > 'threshold' then we assign a 1 for that second in 'th_arr'. Otherwise, it is a 0.
#     2.3. we define a 3-second sliding window, such as <s_1, s_2, s_3>.
#     2.4. the binaary code for <th_arr[s_1], th_arr[s_2], th_arr[s_3]> is read. E.g., <1, 0, 0>
#     2.5. we have a dictionary for 3-digit binaries. E.g., {<0, 0, 0>: A} and {<0, 0, 1>: B}, etc.
#     2.6. the three-second data is converted into one 'word'.
#     2.7. the 3-second window is shifted by 2 seconds. (we keep a 1-second overlapping point)
#     2.9. the process is repeated (go to 2.4) unless we reach the last second of the task.
#
# Input:
#      1. PID: participant unique identifier
#      2. Task: task title
#      3. METs: MET score (energy expenditure) for the given task
#      4. taskDataFrame: accelerometer data.frame. This data.frame should contain a VM column for vector magnitude.
#      5. start.idx: indicates from which point we should check the taskDataFrame$VM
#      6. end.idx: indicates at which point the task ends in the taskDataFrame$VM
#      7. threshold: indicating the threshold which is used for 0-1 encoding of the accelerometer data.
#      8. word.dictionary: a dictionary for converting a 3-second data into a word.
#      9. sampling.rate: how many data points exist for a second
#
# Output:
#      a data.frame with the following columns:
#      1. PID: participant unique identifier
#      2. Task: task title
#      3. start.time: when the task started
#      4. end.time: when the task ended
#      5. BoW: bag of words for the given task
#      6. METs: MET score (energy expenditure) for the given task
BoW.construction <- function(PID, Task, METs, taskDataFrame, start.idx, end.idx, threshold = 3, word.dictionary, sampling.rate = 100) {
     result <- data.frame(matrix(nrow = 0, ncol = 6))
     if(length(start.idx) == 0 || length(end.idx) == 0) {
          return(result)
     }
     if(end.idx < start.idx) {
          print(paste(PID, Task, "Start time is after end time?!",sep = " - "))
          return(result)
     }
     
     # creating 'th_arr' in O(n).
     th_arr <- rep('0', ceiling((end.idx - start.idx + 1) / sampling.rate))
     j <- 1
     for(i in seq(start.idx, end.idx, by = sampling.rate)) {
          pts_ge_threshold <- which(taskDataFrame$VM[i:(i + sampling.rate - 1)] > threshold)
          if(length(pts_ge_threshold) > 0) {
               th_arr[j] <- '1'
          }
          j <- j + 1
     }
     
     # converting 'th_arr' to bag.of.words in O(n)
     Bag.of.Words <- c()
     i <- 1
     while(i + 2 <= j) {
          sliding_window <- th_arr[i:(i + 2)]
          code <- paste(sliding_window, collapse = "")
          word <- as.character(word.dictionary$word[word.dictionary$code == code])
          Bag.of.Words <- c(Bag.of.Words, word)
          i <- i + 2
     }
     Bag.of.Words <- as.character(paste(Bag.of.Words, collapse = ""))
     result <- data.frame(PID = PID, Task = Task, start.time = taskDataFrame$Timestamp[start.idx], end.time = taskDataFrame$Timestamp[end.idx],
                          BoW = Bag.of.Words, METs = METs)
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
#     1. returns a data.frame which contains Bag-of-Words for each <Participant, Task, Epoch>.
BoW.oneParticipant <- function(participantID, cosmed.df, ppt.v1.df, ppt.v2.df, ppt.v3.df, ppt.v4.df, taskTimes.df) {
     message(paste("Constructing Bag-of-Words for", participantID, " started."))
     
     word.dictionary <- data.frame(word = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                                   code = c('000', '001', '010', '011', '100', '101', '110', '111'))
     word.dictionary$word <- as.character(word.dictionary$word); word.dictionary$code <- as.character(word.dictionary$code)
     
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
          if(is.na(taskDataFrame)) {
               message(paste("Skipping ", participantID, "-", Task, " (", visit, "). Since there is no visit file.", sep = ""))
               next()
          } else {
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
                    
                    feature.df <- BoW.construction(PID = participantID, Task = taskTimes.ppt.df$task[1], METs, taskDataFrame, start.idx, end.idx, threshold = 3, word.dictionary = word.dictionary)
                    if(nrow(feature.df) > 0) {
                         result <- rbind(result, feature.df)
                    }
               }
          }
     }
     result
}



# Converts a time string (hh:mm:ss) to a numeric value => seconds passed midnight.
# This is a private function used for overlapping tasks check.
p_convert.timeToNumber <- function(timeStr) {
     tokens <- unlist(strsplit(timeStr, split = ":"))
     result <- NA_real_
     if(length(tokens) == 3) {
          result <- as.numeric(as.character(tokens[1])) * 3600 +
               as.numeric(as.character(tokens[2])) * 60 +
               as.numeric(as.character(tokens[3]))
     }
     result
}

# Check for overlapping tasks
# Input:
#      1. taskTimes.df: a data.frame having the following columns:
#          1. Task
#          2. start.time: (str) hh:mm:ss
#          3. end.time:   (str) hh:mm:ss
#          4. Visit
# There is no output.
# But this function prints the tasks which have overlapping times.
print.overlapping.tasks <- function(taskTimes.df) {
     for(v in levels(as.factor(taskTimes.df$visit))) {
          visit.df <- taskTimes.df[taskTimes.df$visit == v, ]
          visit.df$s <- sapply(visit.df$start, FUN = p_convert.timeToNumber)
          visit.df$f <- sapply(visit.df$end, FUN = p_convert.timeToNumber)
          for(i in 1:(nrow(visit.df) - 1)) {
               for(j in (i + 1):nrow(visit.df)) {
                    if (max(visit.df$s[i], visit.df$s[j]) <= min(visit.df$f[i], visit.df$f[j])) {
                         print(paste("Tasks (", visit.df$task[i], ") and (", visit.df$task[j], ") have overlapping times!", sep = ""))
                    }
               }
          }
     }
}