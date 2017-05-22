###########################################################
# Figures to explain BoW approach
# ______________________
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
###########################################################
setwd("~/Workspaces/R workspace/Activity Recognition/pkg03_bow/")

load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/merged_rawBoW_labeled_W3_D32_dtw.Rdata")
load("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Cleaned Data/BoW_W3_D32/codebook_W3_D32.Rdata")

plotATask <- function(data.df, number.of.points = 100) {
     require(reshape2)
     plot.df <- melt(data.df)
     plot.df <- plot.df[plot.df$variable == "V1", ]
     g <- ggplot(data = plot.df[1:number.of.points, ], aes(x = seq_along(variable), y = value))
     g <- g + geom_line(aes(group = "a")) + labs(x = "", y = "") + scale_x_continuous(breaks = NULL, labels = NULL) + scale_y_continuous(breaks = NULL, labels = NULL)
     g <- g + coord_cartesian(ylim = c(0, 1.5))
     print(g)
}

walking.df <- rawBoW.df[rawBoW.df$PID == "BAVA070", ]
walking.df <- walking.df[walking.df$Task == "LEISURE WALK", c(7:(ncol(walking.df) - 1))]
plotATask(data.df = walking.df, number.of.points = 100)

computer.df <- rawBoW.df[rawBoW.df$PID == "BAVA070", ]
computer.df <- computer.df[computer.df$Task == "COMPUTER WORK", c(7:(ncol(walking.df) - 1))]
plotATask(data.df = computer.df, number.of.points = 100)

sweeping.df <- rawBoW.df[rawBoW.df$PID == "BAVA070", ]
sweeping.df <- sweeping.df[sweeping.df$Task == "SWEEPING", c(7:(ncol(walking.df) - 1))]
plotATask(sweeping.df, number.of.points = 100)



codebook.d1 <- data.frame(x = 1:30, y = computer.df$V1[11:40])
codebook.d2 <- data.frame(x = 1:30, y = sweeping.df$V1[461:490])
codebook.d3 <- data.frame(x = 1:30, y = walking.df$V1[11:40])
codebook.d4 <- data.frame(x = 1:30, y = sweeping.df$V1[111:140])
library(ggplot2)
library(cowplot)
g1 <- ggplot(data = codebook.d1, aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue") + labs(x = "", y = "") + scale_x_continuous(labels = NULL)  + scale_y_continuous(labels = NULL) + coord_cartesian(ylim = c(0, 1.8)) + theme_bw()
g1
g2 <- ggplot(data = codebook.d2, aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue") + labs(x = "", y = "") + scale_x_continuous(labels = NULL)  + scale_y_continuous(labels = NULL) + coord_cartesian(ylim = c(0, 1.8)) + theme_bw()
g2
g3 <- ggplot(data = codebook.d3, aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue") + labs(x = "", y = "") + scale_x_continuous(labels = NULL)  + scale_y_continuous(labels = NULL) + coord_cartesian(ylim = c(0, 1.8)) + theme_bw()
g3
g4 <- ggplot(data = codebook.d4, aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue") + labs(x = "", y = "") + scale_x_continuous(labels = NULL)  + scale_y_continuous(labels = NULL) + coord_cartesian(ylim = c(0, 1.8)) + theme_bw()
g4
plot_grid(g1, g2, g3, g4, ncol = 2)


# --------------- Codebook learning -----------------
library(ggplot2)
library(cowplot)
first.df <- data.frame(x = 1:60, y = computer.df$V1[1:60])
ylimits <- c(min(first.df$y) - 0.1, max(first.df$y) + 0.1)
g1 <- ggplot(data = first.df, aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL)
g1 + coord_cartesian(ylim = ylimits)
sub1.g1 <- ggplot(data = first.df[1:30, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub2.g1 <- ggplot(data = first.df[11:40, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub3.g1 <- ggplot(data = first.df[21:50, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub4.g1 <- ggplot(data = first.df[31:60, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
plot_grid(sub1.g1, sub2.g1, sub3.g1, sub4.g1, ncol = 1)
center1 <- ggplot(data = first.df[11:40, ], aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue", size = 2) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)

second.df <- data.frame(x = 1:60, y = sweeping.df$V1[1:60])
ylimits <- c(min(second.df$y) - 0.1, max(second.df$y) + 0.1)
g2 <- ggplot(data = second.df, aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL)
g2 + coord_cartesian(ylim = ylimits)

sub1.g2 <- ggplot(data = second.df[1:30, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub2.g2 <- ggplot(data = second.df[11:40, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub3.g2 <- ggplot(data = second.df[21:50, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub4.g2 <- ggplot(data = second.df[31:60, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
plot_grid(sub1.g2, sub2.g2, sub3.g2, sub4.g2, ncol = 1)
center2 <- ggplot(data = second.df[11:40, ], aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue", size = 2) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
center3 <- sub4.g2 <- ggplot(data = second.df[31:60, ], aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue", size = 2) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)

third.df <- data.frame(x = 1:60, y = walking.df$V1[1:60])
ylimits <- c(min(third.df$y) - 0.1, max(third.df$y) + 0.1)
g3 <- ggplot(data = third.df, aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL)
g3 + coord_cartesian(ylim = ylimits)
sub1.g3 <- ggplot(data = third.df[1:30, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub2.g3 <- ggplot(data = third.df[11:40, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub3.g3 <- ggplot(data = third.df[21:50, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
sub4.g3 <- ggplot(data = third.df[31:60, ], aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)
plot_grid(sub1.g3, sub2.g3, sub3.g3, sub4.g3, ncol = 1)
center4 <- ggplot(data = third.df[31:60, ], aes(x = x, y = y)) + geom_line(aes(group = "a"), color = "blue", size = 2) + scale_x_continuous(breaks = NULL, labels = NULL) + labs(x = "", y = "") + scale_y_continuous(breaks = NULL, labels = NULL) + coord_cartesian(ylim = ylimits)

plot_grid(center1 + theme_bw(), center2 + theme_bw(), center3 + theme_bw(), center4 + theme_bw(), nrow = 4)


ylimits <- c(min(second.df$y) - 0.1, max(second.df$y) + 0.1)
g4 <- ggplot(data = second.df, aes(x = x, y = y)) + geom_line(aes(group = "a")) + scale_x_continuous(labels = NULL) + labs(x = "", y = "") + scale_y_continuous(labels = NULL)
g4 + coord_cartesian(ylim = ylimits)
