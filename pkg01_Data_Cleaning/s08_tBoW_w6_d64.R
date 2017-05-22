# Codebook for BoW model

# Number of Atoms in the Dictionary
d_t <- 32 # In the head and tail part
d <- 64 # In the body

# How many samples within a minute
numberOfWordsInMinute <- 55 # 55 is for 6-second words


setwd("~/Workspaces/R workspace/Activity Recognition/pkg01_Data_Cleaning/")

# Merging all BoW chunks into one data.frame
dataFolder <- "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/Participant Data/BOW_Files/Six-second chunks//"
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


# separating head and tail from the body of tasks
head_tail.df <- data.frame(matrix(nrow = 0, ncol = ncol(rawBoW.df)))
body.df <- data.frame(matrix(nrow = 0, ncol = ncol(rawBoW.df)))
for(ppt in levels(as.factor(rawBoW.df$PID))) {
     ppt.df <- rawBoW.df[rawBoW.df$PID == ppt, ]
     for(task in levels(as.factor(ppt.df$Task))) {
          task.df <- ppt.df[ppt.df$Task == task, ]
          head_s <- 1
          head_f <- head_s + numberOfWordsInMinute - 1
          tail_f <- nrow(task.df)
          tail_s <- tail_f - numberOfWordsInMinute + 1
          head_tail.df <- rbind(head_tail.df, task.df[head_s:head_f, ], task.df[tail_s:tail_f, ])
          body.df <- rbind(body.df, task.df[(head_f + 1):(tail_s - 1), ])
     }
}
rm(ppt, ppt.df, task, task.df, head_s, head_f, tail_f, tail_s)



# Applying clustering to obtain the codebook.
# We use Partioning around Medioids (PaM) since 1) it is more robust than KMeans and 2) we would like the codebook to contain actual accelerometer data.
library(cluster)
head_tail_atoms <- sapply(1:d_t, FUN = function(x) {as.character(paste("HT", x, sep = ""))})
body_atoms <- sapply(1:d, FUN = function(x) {as.character(paste("D", x, sep = ""))})

# First, we consider only the head_tail part:
head_tail_cluster.df <- head_tail.df[, 7:ncol(head_tail.df)]
set.seed(5855)
head_tail_cluster.outcome <- clara(x = head_tail_cluster.df, k = d_t)
head_tail_codebook <- data.frame(Atom = head_tail_atoms, head_tail_cluster.outcome$medoids, row.names = NULL)
save(head_tail_codebook, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/tBoW_W6_D64/head_tail_codebook_W6_D64.Rdata")
head_tail.df$word.idx <- sapply(as.numeric(head_tail_cluster.outcome$clustering), FUN = function(x) {as.character(paste("HT", x, sep = ""))})

# Second, we consider the body part:
body_cluster.df <- body.df[, 7:ncol(body.df)]
set.seed(5855)
body_cluster.outcome <- clara(x = body_cluster.df, k = d)
body_codebook <- data.frame(Atom = body_atoms, body_cluster.outcome$medoids, row.names = NULL)
save(body_codebook, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/tBoW_W6_D64/body_codebook_W6_D64.Rdata")
body.df$word.idx <- sapply(as.numeric(body_cluster.outcome$clustering), FUN = function(x) {as.character(paste("D", x, sep = ""))})

# Merge them back into one data.frame...
new.df <- data.frame(matrix(nrow = 0, ncol = ncol(body.df)))
for(ppt in levels(as.factor(rawBoW.df$PID))) {
     ppt_head_tail.df <- head_tail.df[head_tail.df$PID == ppt, ]
     ppt_body.df <- body.df[body.df$PID == ppt, ]
     for(task in levels(as.factor(ppt_body.df$Task))) {
          task_head_tail.df <- ppt_head_tail.df[ppt_head_tail.df$Task == task, ]
          task_body.df <- ppt_body.df[ppt_body.df$Task == task, ]
          temp <- rbind(task_head_tail.df, task_body.df)
          temp <- temp[order(temp$Index, decreasing = F), ]
          new.df <- rbind(new.df, temp)
     }
}
rm(ppt, ppt_head_tail.df, ppt_body.df, task, task_head_tail.df, task_body.df, temp)

rawBoW.df <- new.df
rawBoW.df$word.idx <- as.character(rawBoW.df$word.idx)
save(rawBoW.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/tBoW_W6_D64/merged_raw_tBoW_labeled_W6_D64.Rdata")
rm(new.df)

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
          for(word in c(head_tail_atoms, body_atoms)) {
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
numberOfTasks <- nrow(TF.df) / (d + d_t)
IDF.df <- data.frame(Word = c(head_tail_atoms, body_atoms), IDF = NA)
for(word in levels(as.factor(IDF.df$Word))) {
     n_word <- length(which(TF.df$Count[TF.df$Word == word] > 0))
     IDF.df$IDF[IDF.df$Word == word] <- log(numberOfTasks / n_word)
}
rm(word, n_word)



# Final stage of data cleaning
tbow_w6_d64.df <- data.frame(matrix(nrow = 0, ncol = 3 + (d + d_t)))
for(ppt in levels(as.factor(TF.df$PID))) {
     ppt.df <- TF.df[TF.df$PID == ppt, ]
     for(task in levels(as.factor(ppt.df$Task))) {
          task.df <- ppt.df[ppt.df$Task == task, ]
          temp.df <- data.frame(matrix(NA, nrow = 1, ncol = 3 + (d_t + d)))
          colnames(temp.df) <- c("PID", "Task", "MET", c(head_tail_atoms, body_atoms))
          temp.df$PID <- as.character(ppt)
          temp.df$Task <- as.character(task)
          met.df <- rawBoW.df[rawBoW.df$PID == ppt, ]; met.df <- met.df[met.df$Task == task, ]
          temp.df$MET <- met.df$MET[1]
          pptTF.df <- TF.df[TF.df$PID == ppt, ]
          taskTF.df <- pptTF.df[pptTF.df$Task == task, ]
          for(i in 1:(d_t + d)) {
               temp.df[, i + 3] <- taskTF.df$TF[i] * IDF.df$IDF[i]
          }
          tbow_w6_d64.df <- rbind(tbow_w6_d64.df, temp.df)
     }
}
rm(ppt, ppt.df, task, task.df, temp.df, met.df, pptTF.df, taskTF.df, i)
remove.idx <- which(is.na(tbow_w6_d64.df$D1))
tbow_w6_d64.df <- tbow_w6_d64.df[-remove.idx, ]

# Dataset is ready! Save it to a file!
save(tbow_w6_d64.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/tBoW_W6_D64/tBoW_W6_D64.Rdata")



# Plotting the codebook ------------------------------
plot.codebook.W6D64 <- function(codebook, lineSize = 1, lineColor = "blue") {
     require(ggplot2)
     require(cowplot)
     x.labels <- c("0", "1", "2", "3", "4", "5", "6")
     x.breaks <- c(0, 10, 20, 30, 40, 50, 60)
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
     
     item <- 33
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g33 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 34
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g34 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 35
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g35 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 36
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g36 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 37
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g37 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 38
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g38 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 39
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g39 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 40
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g40 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 41
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g41 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 42
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g42 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 43
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g43 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 44
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g44 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 45
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g45 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 46
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g46 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 47
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g47 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 48
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g48 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 49
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g49 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 50
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g50 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 51
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g51 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 52
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g52 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 53
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g53 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 54
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g54 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 55
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g55 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 56
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g56 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 57
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g57 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 58
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g58 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 59
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g59 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 60
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g60 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 61
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g61 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 62
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g62 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 63
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g63 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     item <- 64
     plot.df <- data.frame(x = 1:(ncol(codebook) - 1), t(as.matrix(codebook[item, 2:ncol(codebook)]))); colnames(plot.df) <- c("Deci.Second", "VM")
     g64 <- ggplot(data = plot.df, aes(x = Deci.Second, y = VM)) + geom_line(aes(group = "a"), color = lineColor, size = lineSize) + coord_cartesian(ylim = y.limits) + labs(x = "", y = "") + scale_x_continuous(breaks = x.breaks, labels = x.labels) + theme_bw()
     
     p <- plot_grid(g1, g2, g3, g4, g5, g6, g7, g8,
                    g9, g10, g11, g12, g13, g14, g15, g16,
                    g17, g18, g19, g20, g21, g22, g23, g24,
                    g25, g26, g27, g28, g29, g30, g31, g32,
                    g33, g34, g35, g36, g37, g38, g39, g40,
                    g41, g42, g43, g44, g45, g46, g47, g48,
                    g49, g50, g51, g52, g53, g54, g55, g56,
                    g57, g58, g59, g60, g61, g62, g63, g64,
                    nrow = 8, labels = codebook$Atom)
     print(p)
     p
}

plot.codebook.W6D32 <- function(codebook, lineSize = 1, lineColor = "blue") {
     require(ggplot2)
     require(cowplot)
     x.labels <- c("0", "1", "2", "3", "4", "5", "6")
     x.breaks <- c(0, 10, 20, 30, 40, 50, 60)
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

plot.codebook.W6D32(head_tail_codebook)
plot.codebook.W6D64(body_codebook)


# Plotting a sample conversion ------------------------------
task.df <- rawBoW.df[rawBoW.df$PID == "BAVA070", ]
task.df <- task.df[task.df$Task == "TRASH REMOVAL", ]
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
     g1 <- ggplot(data = data.frame(x = 1:length(VM), y = VM)) + geom_line(aes(x = x, y = y, group = "a"), color = "black", size = 1) + labs(x = "", y = "") + scale_x_continuous(labels = NULL) + scale_y_continuous(labels = NULL)
     g2 <- ggplot(data = data.frame(x = 1:length(newVM), y = newVM)) + geom_line(aes(x = x, y = y, group = "b"), color = "blue", size = 1) + labs(x = "", y = "") + scale_x_continuous(labels = NULL) + scale_y_continuous(labels = NULL)
     p <- plot_grid(g1, g2, nrow = 2)
     print(p)
     p
}
plot.conversionSample(task.df, rbind(head_tail_codebook, body_codebook))
