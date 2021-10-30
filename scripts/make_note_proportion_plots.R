############################
# Make boxplots for figure 3 of Bhat et.al. 2021
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Sat Oct 30 11:38:04 2021
###########################

library(ggplot2)
library(ggpubr)

pseudo_path <- 'data/pseudo/notetype_proportion_data.csv'
nycti_path <- 'data/nycti/notetype_proportion_data.csv'

path <- pseudo_path #change path accordingly
data <- read.csv(path,header=TRUE)


if (path == pseudo_path){
  
  #exclude repeated individuals
  data <- data[which((data$ID != 'MZ001015')),]
  data <- data[which((data$X != 27)),]
  
  #make the plots
  comparisons <- list(c('A','N_A'),c('A','T_D'),c('N_A','T_D'))
  symbols <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  p1 <- ggplot(data,aes(x=context,y=type_1_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15')) + theme(legend.position = 'none') + ylab("Proportion of Type 1 notes")
  p2 <- ggplot(data,aes(x=context,y=type_2_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))  + theme(legend.position = 'none') + ylab("Proportion of Type 2 notes")
  p3 <- ggplot(data,aes(x=context,y=type_3_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15')) + theme(legend.position = 'none') + ylab("Proportion of Type 3 notes")
  p4 <- ggplot(data,aes(x=context,y=type_4_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15')) + theme(legend.position = 'none') + ylab("Proportion of Type 4 notes")
  p5 <- ggplot(data,aes(x=context,y=type_5_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15')) + theme(legend.position = 'none') + ylab("Proportion of Type 5 notes")
  p6 <- ggplot(data,aes(x=context,y=type_6_prop,fill=context)) + stat_boxplot(geom='errorbar') + geom_boxplot() + stat_compare_means(comparisons = comparisons,label.y=c(0.9,1.1,1.3),symnum.args = symbols) +ylim(0,1.5) + stat_compare_means(label.y=1.4)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15')) + theme(legend.position = 'none') + ylab("Proportion of Type 6 notes")
  
  plotlist <- list(p1,p4,p2,p5,p3,p6)
  
  save_path <- 'path_to_file' #Change accordingly
  for (i in seq(6)){
    filename <- paste("pseudo_notetype_",i,".svg",sep='')
    #ggsave(paste(save_path,filename,sep=''),plotlist[[i]],dpi=500)
  }
} else {
  comparisons <- list(c('A','N_A'))
  symbols <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  p_DN <- ggplot(data=data, aes(x = context, y = DN_prop, fill = context)) + stat_boxplot(geom = 'errorbar',width=0.25) + geom_boxplot(width = 0.25) + xlab('Context') + ylab('Proportion of DNs') + scale_fill_manual(values = c("#66FFFF", "#FF6666")) + theme(legend.position = 'none')
  p_DN <- p_DN + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))
  p_DN <- p_DN + ylim(c(0,1)) +  stat_compare_means(comparisons=comparisons,label.y=c(0.75),symnum.args = symbols) + stat_compare_means(label.y=0.9) + theme(legend.position = 'none')
  
  save_path <- 'path_to_file' #Change accordingly
  #ggsave(paste(save_path,'nycti_boxplot.svg',sep=''),p_DN,dpi=500)
}

