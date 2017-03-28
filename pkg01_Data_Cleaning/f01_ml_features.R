# Functions for constructing features.



# This function receives a transformed vector and produces an informative data.frame for analysis.
# Example:
#     freq.df <- convrt.fft(fft(VM)) 
convert.fft <- function(cs, sample.rate = 100) {
     cs <- cs / length(cs) # normalize
     
     distance.center <- function(c) signif(Mod(c), 4)
     angle           <- function(c) signif(180 * Arg(c) / pi, 3)
     
     df <- data.frame(cycle    = 0:(length(cs)-1),
                      freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                      strength = sapply(cs, distance.center),
                      delay    = sapply(cs, angle))
     df
}

# Receives vector magnitude and converts it to frequency domain. Then calculates the fraction of power covered by 0.6 Hz to 2.5 Hz.
p625 <- function(VM, sample.rate = 100) {
     VM_freq <- convert.fft(fft(VM), sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     idx0_6 <- min(which(signif(VM_freq$freq, digits = 1) == 0.6))
     idx2_5 <- min(which(signif(VM_freq$freq, digits = 2) == 2.5))
     result <- sum(VM_freq$strength[idx0_6:idx2_5]) / sum(VM_freq$strength[2:(nrow(VM_freq)-1)])
     result
}

# Receives vector magnitude and converts it to frequency domain. Find the dominant frequency.
df <- function(VM, sample.rate = 100) {
     VM_freq <- convert.fft(fft(VM), sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     temp_idx <- min(which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)])))
     if(length(temp_idx) > 0) {
          idx_max <- temp_idx
     }
     result <- VM_freq$freq[idx_max]
     
     result
}

# Converts the given vector magnitude and finds the dominant frequency. Returns the fraction of power covered by DF.
fpdf <- function(VM, sample.rate = 100) {
     VM_freq <- convert.fft(fft(VM), sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     
     idx_max <- which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)]))
     indices <- which(round(VM_freq$freq, digits = 1) == round(VM_freq$freq[idx_max], digits = 1))
     result <- sum(VM_freq$strength[indices]) / sum(VM_freq$strength[2:(nrow(VM_freq))])
     
     result
}

# Returns the average angle between the vertical axis and the vector magnitude.
mangle <- function(x, VM) {
     angle <- (90 * asin(x / VM)) / (pi/2)
     result <- mean(angle, na.rm = T)
     result
}

# Returns the standard deviation of the angle between the vertical axis and the vector magnitude.
sdangle <- function(x, VM) {
     angle <- (90 * asin(x / VM)) / (pi/2)
     result <- sd(angle, na.rm = T)
     result
}

# Average of vector magnitude.
mvm <- function(VM) {
     result <- mean(VM, na.rm = T)
     result
}

# Standard deviation of vector magnitude.
sdvm <- function(VM) {
     result <- sd(VM)
     result
}

# All the ML features for one epoch
ml.features.oneEpoch <- function(PID, Task, start.time, end.time, x.axis, VM, sample.rate = 100) {
     options(warn = -1)
     feature1 <- mvm(VM)
     feature2 <- sdvm(VM)
     feature3 <- mangle(x.axis, VM)
     feature4 <- sdangle(x.axis, VM)
     feature5 <- p625(VM, sample.rate)
     feature6 <- df(VM, sample.rate)
     feature7 <- fpdf(VM, sample.rate)
     result <- data.frame(PID = PID,
                          Task = Task,
                          start.time = start.time,
                          end.time = end.time,
                          MVM = feature1,
                          SDVM = feature2,
                          MANGLE = feature3,
                          SDANGLE = feature4,
                          P625 = feature5,
                          DF = feature6,
                          FPDF = feature7)
     options(warn = 0)
     result
}

# For every non-overlapping epoch (default: 15 seconds), constructs a feature vector with the following columns:
#     PID: participant ID
#     Task
#     MVM: average of VM
#     SDVM: standard deviation of VM
#     MANGLE: average of the degree between vertical axis and VM
#     SDANGLE: standard deviation of the degree between vertical axis and VM
#     P625: fraction of power covered by frequencies between 0.6 Hz and 2.5 Hz
#     DF: dominant frequency
#     FPDF: fraction of power covered by DF
#     MET: MET score for the performed task
ml.featureConstruction <- function(PID, Task, METs, taskDataFrame, start.idx, end.idx, epoch.length = (15 * 100)) {
     result <- data.frame(matrix(nrow = 0, ncol = 12))
     if(length(start.idx) == 0 || length(end.idx) == 0) {
          return(result)
     }
     if(end.idx < start.idx) {
          print(paste(PID, Task, "Start time is after end time?!",sep = " - "))
          return(result)
     }
     for(i in seq(start.idx, end.idx, by = epoch.length)) {
          last.idx <- min(nrow(taskDataFrame), (i + epoch.length - 1))
          curResult <- ml.features.oneEpoch(PID, Task, taskDataFrame$timeOnly[i], taskDataFrame$timeOnly[last.idx], taskDataFrame$X[i:last.idx], taskDataFrame$VM[i:last.idx], 100)
          curResult$MET <- METs
          options(warn = -1)
          if(!is.na(curResult)) {
               result <- rbind(result, curResult)
          }
          options(warn = 0)
     }
     result
}


