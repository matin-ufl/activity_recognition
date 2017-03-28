###
# Getting a list of participant to use in on our studies.
###

setwd("~/Workspaces/R workspace/Activity Recognition/f01/")

library(readstata13)
a <- read.dta13("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/cosmed_mets_tread_rmr_data-3.10.2017.dta")
remove.idx <- which(a$bad_cosmed_data == 1)
print(paste(length(remove.idx), "participants have bad_cosmed_data. REMOVED"))
a <- a[-remove.idx, ]
rm(remove.idx)

a$participant_acrostic <- factor(a$participant_acrostic)

# sorting based on age and paricipant code
cosmed_sorted_age <- a[with(order(cosmed_age, participant_acrostic, decreasing = F), data = a), ]

# Selecting participants from their ages
ages <- factor(cosmed_sorted_age$cosmed_age)
print(paste("There are", length(levels(ages)), "different ages in our dataset."))

ppt_age.df <- data.frame(matrix(nrow = 0, ncol = 2))
for (age in levels(ages)){
     num_age <- as.numeric(as.character(age))
     print(paste("Participants at age of", num_age, "are:"))
     ppts_selected_by_age <- cosmed_sorted_age[cosmed_sorted_age$cosmed_age == num_age, ]
     ppts_IDs <- levels(factor(ppts_selected_by_age$participant_acrostic))
     print(ppts_IDs)
     for (ppt in ppts_IDs){
          ppt_age.df <- rbind(ppt_age.df, data.frame(Participant = ppt, Age = num_age))
     }
}
write.csv(x = ppt_age.df, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/participant_age.csv", row.names = F)
write.csv(x = cosmed_sorted_age, file = "~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/cosmed_mets.csv", row.names = F)
