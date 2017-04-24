#############################################################
# Participants' characteristics.
# Activity recognition paper.
#
# Matin Kheirkhahan (matinkheirkhahan@ufl.edu)
#############################################################


a <- read.csv("~/Dropbox/Work-Research/Current Directory/Activity Recognition/Datasets/Raw Data/demographics_selected.csv")
a$BMI <- NA
for(i in 1:nrow(a)) {
     a$BMI[i] <- (a$Weight[i]) / ((a$Height[i] / 100)^2)
}
rm(i)


print(paste("Male (", length(which(a$Gender == 'M')), ") vs Female (", length(which(a$Gender == 'F')), ")"))
print(paste("Age: ", round(mean(a$Age), digits = 1), "(", round(sd(a$Age), digits = 1), ")"))
print(paste("BMI: ", round(mean(a$BMI), digits = 1), "(", round(sd(a$BMI), digits = 1), ")"))
print(paste("Height: ", round(mean(a$Height), digits = 1), "(", round(sd(a$Height), digits = 1), ")"))
print(paste("Weight: ", round(mean(a$Weight), digits = 1), "(", round(sd(a$Weight), digits = 1), ")"))
