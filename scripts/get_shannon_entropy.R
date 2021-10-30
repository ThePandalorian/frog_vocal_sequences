############################
# Make figure 4a
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Sat Oct 30 11:34:26 2021
###########################

library(ggplot2)
library(ggpubr)
library(dplyr)

pseudo_path <- 'data/pseudo/notetype_proportion_data.csv'
nycti_path <- 'data/nycti/notetype_proportion_data.csv'

path <- nycti_path #change path accordingly
data <- read.csv(path,header=TRUE) 

shannon <- function(df){
  Hlist = rep(0,nrow(df))
  for (i in 1:nrow(df)){
    H = 0
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){
        H = H + df[i,j]*log(df[i,j])
      }
    }
    Hlist[i] = - H
  }
  return (Hlist)
}

if (path==pseudo_path){
  data %>% select(type_1_prop,type_6_prop) %>% shannon() -> H
} else {
  data %>% select(AN_prop,DN_prop) %>% shannon() -> H
}

data <- cbind(data,H)

if (path == pseudo_path){
  comparisons <- list(c('A','N_A'),c('A','T_D'),c('N_A','T_D'))
  symbols <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  p <- ggplot(data,aes(x=context,y=H,fill=context)) + stat_boxplot(geom='errorbar',width=0.5) + geom_boxplot(width=0.5) + ylab("Shannon Entropy (H)") + xlab("")
  p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))
  p <- p + ylim(c(0,0.89)) +  stat_compare_means(comparisons = comparisons,label.y=c(0.70,0.75,0.82),symnum.args = symbols) + stat_compare_means(label.y=1.7) + theme(legend.position = 'none')
  
  save_path <- 'path_to_file' #Change accordingly
  ggsave(paste(save_path,'pseudo_shannon.svg'),p,dpi=500)
} else {
  comparisons <- list(c('A','N_A'))
  symbols <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
  
  p <- ggplot(data,aes(x=context,y=H,fill=context)) + stat_boxplot(geom='errorbar',width=0.5) + geom_boxplot(width=0.5) + ylab("Shannon Entropy (H)") + xlab("")
  p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(face = 'bold', color = 'black',size = 15)) + theme(axis.title = element_text(face = 'bold',color = 'black',size = '15'))
  p <- p + ylim(c(0,1)) +  stat_compare_means(comparisons = comparisons,label.y=c(0.85),symnum.args = symbols) + stat_compare_means(label.y=1.7) + theme(legend.position = 'none')
  
  save_path <- 'path_to_file' #Change accordingly
  ggsave(paste(save_path,'nycti_shannon.svg'),p,dpi=500)
}


