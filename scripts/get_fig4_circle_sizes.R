############################
# This script was used to obtain the diameters of the circles used 
# in figure 4(b) of Bhat et.al. 2021
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Sat Oct 30 11:30:20 2021
###########################

#We want a monotonic function f(x) for the diameter of the circles. Monotonicity
#ensures that notetypes that are more common have larger circles in the figure.
#I use the function f(x) = alog(bx), where a and b are constants, chosen here 
# such that the figures come out to be reasonably sized
#log is to prevent very large proportions from dominating the figure
#and obscuring all patterns

f <- function(x) {
  y =  3*(log2(100*x))
  if (y > 10){
    return (y)
  }
  else {
    return (6)
  }
}

fvec <- function(vec){
  return (sapply(vec,f))
}

library(dplyr)

nycti_path <- 'data/nycti/notetype_proportion_data.csv'
pseudo_path <- 'data/pseudo/notetype_proportion_data.csv'

path <- nycti_path #change accordingly
proportions <- read.csv(path) 

if (path == pseudo_path){
  proportions %>% 
    group_by(context) %>% 
    summarise_each(funs = mean, type_1_prop, type_2_prop, type_3_prop, type_4_prop, type_5_prop, type_6_prop) %>% 
    summarise_each(funs = fvec, type_1_prop, type_2_prop, type_3_prop, type_4_prop, type_5_prop, type_6_prop) -> radii
} else {
  proportions %>% 
    group_by(context) %>% 
    summarise_each(funs = mean, AN_prop,DN_prop) %>% 
    summarise_each(funs = fvec, AN_prop,DN_prop) -> radii
}





  