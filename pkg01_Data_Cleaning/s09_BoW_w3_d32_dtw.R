# Codebook for BoW model

# Number of Atoms in the Dictionary
d <- 32

setwd("~/Workspaces/R workspace/Activity Recognition/pkg01_Data_Cleaning/")

# Merging all BoW chunks into one data.frame
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/BOW_Files/Three-second chunks/"
l <- dir(path = dataFolder, pattern = "^.*.Rdata$")

allChunks.df <- data.frame(matrix(nrow = 0, ncol = 36))
for(fileName in l) {
     load(paste(dataFolder, fileName, sep = ""))
     allChunks.df <- rbind(allChunks.df, bowChunks.df)
}
rm(fileName, bowChunks.df)

# Removing unnecessary tasks from this raw dataset
keep.these.tasks <- c("LIGHT GARDENING", "YARD WORK", "DIGGING", "LEISURE WALK",
                      "RAPID WALK", "SWEEPING", "PREPARE SERVE MEAL", "STRAIGHTENING UP DUSTING",
                      "WASHING DISHES", "UNLOADING STORING DISHES", "VACUUMING", "WALKING AT RPE 1", 
                      "PERSONAL CARE", "DRESSING", "WALKING AT RPE 5", "STAIR DESCENT",
                      "STAIR ASCENT", "TRASH REMOVAL", "REPLACING SHEETS ON A BED", "STRETCHING YOGA",
                      "MOPPING", "COMPUTER WORK", "LAUNDRY WASHING", "SHOPPING",
                      "IRONING", "LIGHT HOME MAINTENANCE", "WASHING WINDOWS", "HEAVY LIFTING",
                      "STRENGTH EXERCISE LEG CURL", "STRENGTH EXERCISE CHEST PRESS", "STRENGTH EXERCISE LEG EXTENSION", "TV WATCHING",
                      "STANDING STILL")
rawBoW.df <- allChunks.df[allChunks.df$Task %in% keep.these.tasks, ]
rawBoW.df$PID <- as.character(rawBoW.df$PID); rawBoW.df$Task <- as.character(rawBoW.df$Task); rawBoW.df$Start.Time <- as.character(rawBoW.df$Start.Time); rawBoW.df$End.Time <- as.character(rawBoW.df$End.Time)

# Applying clustering to obtain the codebook.
# We use Partioning around Medioids (PaM) since 1) it is more robust than KMeans and 2) we would like the codebook to contain actual accelerometer data.
library(cluster)

# Selecting V1 to V30 (acceleration data) for clustering
cluster.df <- rawBoW.df[, 7:ncol(rawBoW.df)]
set.seed(5855)
cluster.outcome <- clara(x = cluster.df, k = d)
# Codebook is learned.
atoms <- sapply(1:d, FUN = function(x) {as.character(paste("D", x, sep = ""))})
codebook <- data.frame(Atom = atoms, cluster.outcome$medoids, row.names = NULL)
save(codebook, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/codebook_W3_D32.Rdata")

# Labeling the data chunks - We use Dynamic Time Warping for data encoding
library(dtw)

# Dynamic Time Warping Distance to an atom
distanceToAnAtom <- function(atom, sample) {
     y <- as.numeric(as.vector(t(atom[-1])))
     z <- as.vector(t(sample))
     out <- dtw(z, y, distance.only = T)
     out$distance
}

# Find the pattern which is closest to the current epoch using DTW distance
whichCluster <- function(sample, codebook) {
     distances <- apply(codebook, FUN = distanceToAnAtom, sample = sample, MARGIN = 1)
     clusterLabel <- as.character(codebook$Atom[which.min(distances)])
}

#word.idx <- apply(rawBoW.df[, -c(1:6)], FUN = whichCluster, codebook = codebook, MARGIN = 1)
pb <- txtProgressBar(min = 1, max = nrow(rawBoW.df), style = 3)
word.idx <- rep(NA, nrow(rawBoW.df))
for(i in 1:nrow(rawBoW.df)) {
     sample <- rawBoW.df[i, -c(1:6)]
     word.idx[i] <- whichCluster(sample, codebook)
     if(i %% 1000 == 0) {
          message(paste(i, "out of", nrow(rawBoW.df)))
          setTxtProgressBar(pb, value = i)
     } 
}
close(pb)
rm(i, pb, sample)
rawBoW.df$word.idx <- word.idx
save(rawBoW.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/merged_rawBoW_labeled_W3_D32_dtw.Rdata")


# Calculating TF_IDF for each ppt-task
justWords.df <- rawBoW.df[, c(1:5, ncol(rawBoW.df))]

# First, term frequency (TF): we use augmented frequency to prevent a bias towards longer documents.
TF.df <- data.frame(matrix(nrow = 0, ncol = 4))
for(ppt in levels(as.factor(justWords.df$PID))) {
     ppt.df <- justWords.df[justWords.df$PID == ppt, ]
     for(task in levels(as.factor(ppt.df$Task))) {
          task.df <- ppt.df[ppt.df$Task == task, ]
          numberOfWords <- nrow(task.df)          
          curTF.df <- data.frame(matrix(nrow = 0, ncol = 2))
          for(word in atoms) {
               curTF.df <- rbind(curTF.df, data.frame(TF = length(which(task.df$word.idx == word)), Word = word))
          }
          maxTF <- max(curTF.df$TF)
          for(word in curTF.df$Word) {
               count <- curTF.df$TF[curTF.df$Word == word]
               augmented.frequency = 0.5 + 0.5 * (count / maxTF)
               TF.df <- rbind(TF.df,
                              data.frame(PID = ppt, Task = task, Word = word, Count = count,  TF = augmented.frequency))
          }
          
     }
}
rm(maxTF, curTF.df, count, augmented.frequency, ppt.df, task.df, word, ppt)

# Second, inverse document frequency (IDF) is calculated.
numberOfTasks <- nrow(TF.df) / d
IDF.df <- data.frame(Word = atoms, IDF = NA)
for(word in levels(as.factor(IDF.df$Word))) {
     n_word <- length(which(TF.df$Count[TF.df$Word == word] > 0))
     IDF.df$IDF[IDF.df$Word == word] <- log(numberOfTasks / n_word)
}
rm(word, n_word)



# Final stage of data cleaning
bow_w3_d32_dtw.df <- data.frame(matrix(nrow = 0, ncol = 3 + d))
for(ppt in levels(as.factor(TF.df$PID))) {
     ppt.df <- TF.df[TF.df$PID == ppt, ]
     for(task in levels(as.factor(ppt.df$Task))) {
          task.df <- ppt.df[ppt.df$Task == task, ]
          temp.df <- data.frame(matrix(NA, nrow = 1, ncol = 3 + d))
          colnames(temp.df) <- c("PID", "Task", "MET", atoms)
          temp.df$PID <- as.character(ppt)
          temp.df$Task <- as.character(task)
          met.df <- rawBoW.df[rawBoW.df$PID == ppt, ]; met.df <- met.df[met.df$Task == task, ]
          temp.df$MET <- met.df$MET[1]
          pptTF.df <- TF.df[TF.df$PID == ppt, ]
          taskTF.df <- pptTF.df[pptTF.df$Task == task, ]
          for(i in 1:d) {
               temp.df[, i + 3] <- taskTF.df$TF[i] * IDF.df$IDF[i]
          }
          bow_w3_d32_dtw.df <- rbind(bow_w3_d32_dtw.df, temp.df)
     }
}
rm(ppt, ppt.df, task, task.df, temp.df, met.df, pptTF.df, taskTF.df, i)
remove.idx <- which(is.na(bow_w3_d32_dtw.df$D1))
bow_w3_d32_dtw.df <- bow_w3_d32_dtw.df[-remove.idx, ]

# Dataset is ready! Save it to a file!
save(bow_w3_d32_dtw.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/BoW_W3_D32_dtw.Rdata")



# Plotting the codebook ------------------------------
plot.codebook.W3D32 <- function(codebook, lineSize = 1, lineColor = "blue") {
     require(ggplot2)
     require(cowplot)
     x.labels <- c("0", "1", "2", "3")
     x.breaks <- c(0, 10, 20, 30)
     y.limits <- c(min(codebook[, 2:ncol(codebook)]) - 0.1, max(codebook[, 2:ncol(codebook)]) + 0.1)
     item <- 1
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g1 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 2
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g2 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 3
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g3 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 4
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g4 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 5
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g5 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 6
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g6 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 7
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g7 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 8
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g8 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 9
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g9 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 10
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g10 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 11
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g11 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 12
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g12 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 13
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g13 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 14
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g14 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 15
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g15 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 16
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g16 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 17
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g17 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 18
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g18 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 19
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g19 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 20
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g20 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 21
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g21 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 22
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g22 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 23
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g23 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 24
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g24 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 25
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g25 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 26
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g26 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 27
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g27 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 28
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g28 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 29
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g29 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 30
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g30 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 31
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g31 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 32
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g32 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     p <- plot_grid(g1, g2, g3, g4, g5, g6, g7, g8,
                    g9, g10, g11, g12, g13, g14, g15, g16,
                    g17, g18, g19, g20, g21, g22, g23, g24, g25,
                    g26, g27, g28, g29, g30, g31, g32, nrow = 4, labels = codebook$Atom)
     print(p)
     p
}

plot.codebook.W3D32(codebook)


# Plotting a sample conversion ------------------------------
task.df <- rawBoW.df[rawBoW.df$PID == "BAVA070", ]
task.df <- task.df[task.df$Task == "LEISURE WALK", ]
plot.conversionSample <- function(task.df, codebook) {
     VM <- c()
     newVM <- c()
     for(i in 1:(nrow(task.df) - 1)) {
          VM <- c(VM, t(as.matrix(task.df[i, 7:36])))
          word <- task.df$word.idx[i]
          newVM <- c(newVM, t(as.matrix(codebook[codebook$Atom == word, 2:ncol(codebook)])))
     }
     VM <- c(VM, t(as.matrix(task.df[nrow(task.df), 7:36])))
     word <- task.df$word.idx[nrow(task.df)]
     newVM <- c(newVM, t(as.matrix(codebook[codebook$Atom == word, 2:ncol(codebook)])))
     rm(i, word)
     g1 <- ggplot(data = data.frame(x = 1:length(VM), y = VM)) + geom_line(aes(x = x, y = y, group = "a"), color = "black") + labs(x = "", y = "") + scale_x_continuous(labels = NULL) + scale_y_continuous(labels = NULL)
     g2 <- ggplot(data = data.frame(x = 1:length(newVM), y = newVM)) + geom_line(aes(x = x, y = y, group = "b"), color = "blue") + labs(x = "", y = "") + scale_x_continuous(labels = NULL) + scale_y_continuous(labels = NULL)
     p <- plot_grid(g1, g2, nrow = 2)
     print(p)
     p
}
plot.conversionSample(task.df, codebook)
