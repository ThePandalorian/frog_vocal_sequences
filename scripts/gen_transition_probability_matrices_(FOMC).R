############################
# Code for estimating transition matrices for a first order markov chain
# And running statistical tests on the transition matrices obtained.
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Fri Sep 24 14:55:52 2021
###########################

library(stringr)

#gen_avg_MC generates a single pairwise transition probability matrix for a given context, using a formula derived from MLE

gen_avg_MC <- function(df,num=7){ 
  #num is the number of notetypes present in the organism
  #including silence (silence is coded for as notetype 0)
  
  avg_matrix <- data.frame(matrix(rep(0,(num)**2),nrow = num,ncol=num))
  marginalproblist <- rep(0,num)
  for (i in seq(num+1)){ #add 1 and subtract when searching because R indexes from 1 and not from 0 (so notetype 0 will raise an error)
    marginalprob <- 0
    for (j in seq(num+1)){ #Count the number of times an (i-1) --> _ transition occurred
      marginalprob <- marginalprob + sum(str_count(df$sequence,pattern=paste(as.character(i-1),as.character(j-1),sep='')))
    }
    marginalproblist[i] <- marginalprob #Add to marginal probabilities list
  }
  marginalproblist <- marginalproblist[1:num] #To remove the last (spuriously generated) element
  for (i in seq(nrow(df))){
    seq <- df$sequence[i]
    statespace <- as.numeric(unique(unlist(str_split(seq,pattern=''))))

    for (j in statespace+1){ #add 1 and subtract when searching because R indexes from 1 and not from 0 (so notetype 0 will raise an error)
      if (marginalproblist[j]==0){
        next
      }
      else{
        for (k in statespace+1){ #Count the number of times a (j-1) --> (k-1) transition occurred
          t_jk <- sum(str_count(seq,pattern = paste(as.character(j-1),as.character(k-1),sep='')))/marginalproblist[j]
          avg_matrix[j,k] <- avg_matrix[j,k] + t_jk
        }
      }
    }
  }

  rownames <- rep('a',num)
  colnames <- rep('b',num)
  for (i in 1:num){ #Just to give the rows and columns informative names
    rownames[i] <- paste('t',as.character(i-1),'_',sep='')
    colnames[i] <- paste('t','_',as.character(i-1),sep='')
  }

  rownames(avg_matrix) <- rownames
  colnames(avg_matrix) <- colnames
  
  #Return marginal probabilities, transition matrix, sample size of notes
  return (list('pi'=marginalproblist/sum(marginalproblist),'T'=avg_matrix,'n'=sum(marginalproblist)))
}


#######################################################
#Stats
#I use a phi-divergence test
#I use the specific function phi(x) = - log(x) 
#This makes the divergence function the KL divergence
#Other phi-divergences can also be used here

#The phi function for the corresponding phi-divergence
phi <- function(x) { #I use phi(x) = -log(x), corresponding to KL-divergence
  if (x != 0 & !is.nan(x) & !is.infinite(x)){ #To avoid errors due to zeroes
    return (-log(x))
  }
  else{
    return (0)
  }
}

#double derivative of phi (for the phi-divergence test)
#Remember to change this if you change the phi function above!
ddphi <- function(x){
  return (1/(x**2))
}

#The below function runs a homogeneity test based on the m.d.i.s
#Reference:
#Kullback et.al. 1962 - Tests for contingency tables and Markov chains
#This is the test used in the Bhat et.al. paper
kullback_homogeneity_test <- function(MC_1,MC_2){
  
  # MC_1 and MC_2 are two outputs of the gen_avg_mc function
  # H0: both processes have the same underlying transition matrices
  # Refer to section 9 and table 9.1 of Kullback et.al. 1962
  
  #Multiply by n because the test uses direct counts
  T1 <- MC_1[['n']]*MC_1[['T']]
  T2 <- MC_2[['n']]*MC_2[['T']]
  
  #Directly multiplying by n would overcount. We need the number of
  #times notetype i occurred (i.e n_i). But n_i = n*(pi_i) and we already
  #multiplied by n in the previous step. #Therefore, multiplying the current
  #values by pi_i gives us the correct counts.
  
  for (i in 1:nrow(T1)){
    for (j in 1:nrow(T1)){
      T1[i,j] <- T1[i,j]*MC_1[['pi']][i]
      T2[i,j] <- T2[i,j]*MC_2[['pi']][i]
    }
  }
  
  f <- list(T1,T2) #To get f_{ijk} values
  
  #Calculate necessary quantities
  
  f1 <- MC_1[['n']]
  f2 <- MC_2[['n']]
  fi_sum <- c(f1,f2)#This is f_{i..}
  
  
  f1j <- MC_1[['n']]*MC_1[['pi']]
  f2j <- MC_1[['n']]*MC_2[['pi']]
  fij_sum <- list(f1j,f2j)#This is f_{ij.}
  
  # n = f_{...} = (no. of entries) - s
  n <- MC_1[['n']] + MC_2[['n']] - 2
  
  statistic <- 0 
  
  #The statistic is that corresponding to 'conditional homogeneity' in table 9.1
  #note that for us, since i = 1,2,
  #f_{.jk} = f_{1jk} + f_{2jk} and f_{.j.} = f_{1j.} + f_{2j.} by definition
  for (i in 1:2){
    for (j in 1:nrow(T1)){
      for (k in 1:nrow(T1)){
        phi_term_num <- f[[i]][j,k]
        phi_term_den <- fij_sum[[i]][j]*(f[[1]][j,k]+f[[2]][j,k])/(fij_sum[[1]][j]+fij_sum[[2]][j])
        phi_term <- phi_term_num/phi_term_den
        
        #I use -phi because phi is defined as -log (and we want log here)
        statistic <- statistic + f[[i]][j,k]*(-phi(phi_term))
      }
    }
  }
  
  statistic <- 2*statistic
  
  # This test statistic is asymptotically chi-square with df = r(r-1) (s=2 for us)
  df = nrow(T1)*(nrow(T1)-1)
  
  #We can estimate the corresponding p-value using the pchisq function
  #lower.tail = False lets you look for values as extreme or more extreme than
  #the specified value, i.e Pr(X >= statistic)
  #By definition, this is the p-value
  
  p_value = pchisq(statistic,df=df,lower.tail = FALSE)
  
  return(list('statistic'=statistic,'df'=df,'p_value'=p_value))
}

#phi-divergence test for similarity of two transition matrices
# References:
# 1. Billingsley 1961 - Statistical methods in Markov chains
# 2. Menendez et.al. 1999 - Statistical inference for finite Markov chains 
#    based on divergences
# 3. Page 139 of Pardo 2006 - Statistical Inference Based on Divergence Measures

# These authors consider one sample tests with the hypothesis P^ = P assuming P known
#This is not really suitable for the hypothesis we are testing (which is two-sample)
#But I have seen it used before, so just adding the function here for completeness
#The kullback test is greatly preferable (makes much more sense) and so that
#is the one we actually use in the paper
phi_divergence_test <- function(MC_1,MC_2){
  
  # MC_1 and MC_2 are two outputs of the gen_avg_mc function
  # Matrix estimated with fewer samples should be second
  #H0: T1 = T2
  
  T1 <- MC_1[['T']]
  pi1 <- MC_1[['pi']]
  
  T2 <- MC_2[['T']]
  
  statistic <- 0
  
  for (i in 1:nrow(T1)){
    for (j in 1:nrow(T1)){
      statistic <- statistic + (pi1[i])*T1[i,j]*phi(T2[i,j]/T1[i,j])
    }
  }
  
  statistic <- (2*(MC_2[['n']])/ddphi(1))*statistic
  
  # This test statistic is asymptotically chi-square with df = c - M, 
  # where c is the number of non-zero entries in the transition matrix T1 
  # and M is the size of the state space of the process
  
  #We can estimate the corresponding p-value using the pchisq function
  #lower.tail = False lets you look for values as extreme or more extreme than
  #the specified value, i.e Pr(X>statistic)
  #By definition, this is the p-value
  
  df = sum(T1 != 0)-nrow(T1)
  
  p_value = pchisq(statistic,df=df,lower.tail = FALSE)
  
  return(list('statistic'=statistic,'df'=df,'p_value'=p_value))
}

##############################################################################
#Make the matrices
 
nycti_homepath <-  'data/nycti/seqdata_sil.csv'
pseudo_homepath <- 'data/pseudo/seqdata_sil.csv'

homepath <- pseudo_homepath #modify according to what you want

seq <- read.csv(homepath,header = FALSE)
if (homepath == pseudo_homepath){
  seq <- seq[2:51,]
  colnames(seq) <- c('sl_no','ID','context','sequence')
  seq$sequence <- as.character(seq$sequence)
  seq$context <- as.character(seq$context)
  seq$ID <- as.character(seq$ID)
  
  MC_A <- gen_avg_MC(seq[which(seq$context == 'A'),])
  MC_NA <- gen_avg_MC(seq[which(seq$context == 'N_A'),])
  MC_TD <- gen_avg_MC(seq[which(seq$context == 'T_D'),])
} else {
  seq <- seq[2:20,]
  colnames(seq) <- c('sl_no','ID','context','sequence')
  seq$sequence <- as.character(seq$sequence)
  seq$context <- as.character(seq$context)
  seq$ID <- as.character(seq$ID)
  
  MC_A <- gen_avg_MC(seq[which(seq$context == 'A'),],num=3)
  MC_NA <- gen_avg_MC(seq[which(seq$context == 'N_A'),],num=3)
}

#To run stats, simply call the test function you want and supply the two MC
#objects as arguments. 

# Ex: for the homogeneity test between A and N_A, simply execute the below line
#kullback_homogeneity_test(MC_A,MC_NA)
###############################################################################
