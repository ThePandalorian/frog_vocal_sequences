############################
# Check whether the call rate of N. humayuni varies with context
# Written by Shikhara Bhat
# IISER Pune, India
# Last modified: Sat Oct 30 11:24:03 2021
###########################

data <- read.csv("data/nycti/call_rate.csv", header = TRUE)
T <- rep(0,19)
for (i in 1:19) {
   T[i] <- 60*data$Recording.time..mins.[i] + data$Recording.time..seconds.[i]
}
ANrate <- rep(0,19)
DNrate <- rep(0,19)
totalrate <- rep(0,19)
for (i in 1:19) {
   ANrate[i] <- 60*data$AN[i]/T[i]
   DNrate[i] <- 60*data$DN[i]/T[i]
   totalrate[i] <- 60*(data$AN[i]+data$DN[i])/T[i]
}
data <- cbind(data,ANrate,DNrate,totalrate)
lone <- data[which(data$Group == 'Lone Male'),]
nbr <- data[which(data$Group == 'Single Neighbour'),]
