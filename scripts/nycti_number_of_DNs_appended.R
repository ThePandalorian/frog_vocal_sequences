############################
# Make the plot in figure 3(b) of Bhat et.al. 2021
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Sat Oct 30 11:42:01 2021
###########################

path_to_csv <- 'data/nycti/prob_appending_DNs.csv'
data <- read.csv(path_to_csv, sep = ',', header = TRUE)
colnames(data) <- c('State','0DN','1DN','2DN','3DN','4DN','5DN','6DN')

prob0 <- rep(0,20)
for (i in 1:20) {
        prob0[i] <- (data$`0DN`[i])/(rowSums(data[,2:7])[i])
}
data <- cbind(data,prob0)

prob1 <- rep(0,20)
for (i in 1:20) {
     prob1[i] <- (data$`1DN`[i])/(rowSums(data[,2:7])[i])
 }
data <- cbind(data,prob1)

prob2 <- rep(0,20)
for (i in 1:20) {
     prob2[i] <- (data$`2DN`[i])/(rowSums(data[,2:7])[i])
 }
data <- cbind(data,prob2)

prob3 <- rep(0,20)
for (i in 1:20) {
     prob3[i] <- (data$`3DN`[i])/(rowSums(data[,2:7])[i])
 }
data <- cbind(data,prob3)

prob4 <- rep(0,20)
for (i in 1:20) {
     prob4[i] <- (data$`4DN`[i])/(rowSums(data[,2:7])[i])
 }
data <- cbind(data,prob4)

y2 <- c(mean(data[which(data$State == 'Single Neighbour'),]$prob0),mean(data[which(data$State == 'Single Neighbour'),]$prob1),mean(data[which(data$State == 'Single Neighbour'),]$prob2),mean(data[which(data$State == 'Single Neighbour'),]$prob3),mean(data[which(data$State == 'Single Neighbour'),]$prob4),0)
y1 <- c(mean(data[which(data$State == 'Lone Male'),]$prob0),mean(data[which(data$State == 'Lone Male'),]$prob1),mean(data[which(data$State == 'Lone Male'),]$prob2),mean(data[which(data$State == 'Lone Male'),]$prob3),mean(data[which(data$State == 'Lone Male'),]$prob4),0)
x <- c(0:5)
sd2 <- c(sd(data[which(data$State == 'Single Neighbour'),]$prob0),sd(data[which(data$State == 'Single Neighbour'),]$prob1),sd(data[which(data$State == 'Single Neighbour'),]$prob2),sd(data[which(data$State == 'Single Neighbour'),]$prob3),sd(data[which(data$State == 'Single Neighbour'),]$prob4),0)
sd1 <- c(sd(data[which(data$State == 'Lone Male'),]$prob0),sd(data[which(data$State == 'Lone Male'),]$prob1),sd(data[which(data$State == 'Lone Male'),]$prob2),sd(data[which(data$State == 'Lone Male'),]$prob3),sd(data[which(data$State == 'Lone Male'),]$prob4),0)

data <- data.frame(x,y1,y2,sd1,sd2)
library("ggplot2")
p <- ggplot(data, aes(x=x,y=y1)) + geom_line(aes(color='Lone Male')) + geom_point(aes(colour = "Lone Male")) + geom_errorbar(aes(ymin=y1-sd1,ymax = y1+sd1,color='Lone Male'),width=0.1)
p <- p + geom_line(aes(x=x,y=y2,colour = 'Male with one neighbour')) + geom_point(aes(x=x,y=y2,colour = 'Male with one neighbour')) +  geom_errorbar(aes(ymin=y2-sd2,ymax = y2+sd2,color='Male with one neighbour'),width=0.1)
p <- p + theme_bw() + ylab("Proportion of notes") + xlab("Number of DNs appended") + labs(colour = "Legend") + scale_colour_manual(name = "Context",values = c("Lone Male" = "red","Male with one neighbour" = 'blue'))
p <- p + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))

save_path <- 'path_to_file' #Change accordingly
ggsave(paste(save_path,'nycti_DN_props.svg',sep=''),p,dpi=500)

