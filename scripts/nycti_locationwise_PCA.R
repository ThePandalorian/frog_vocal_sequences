############################
# Make images for figure A1 of Bhat et.al. 2021
# You can stitch these images together into a gif if you want
# Written by Shikhara Bhat
# IISER Pune, India
# Date: Sat Oct 30 11:40:10 2021
###########################

library(rgl)

data <- read.csv('data/nycti/alldata_mastersheet.csv')

#Get the PCA
data_pca <- prcomp(data[,9:20],scale=TRUE)


#Save PCA loadings to CSV
#For table A1
#write.csv(data.frame(data_pca$rotation),'PCA_loadings.csv')

#Color according to location
col <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  location <- data$location[i]
  if (location == 'matheran'){
    col[i] <- '#0000ff' #Matheran is blue
  }
  else if (location == 'tamhini'){
    col[i] <- '#ff0000' #Adarwadi is red
  }
}

#Create images
plot3d(data_pca$x[,1:3],col=col,size=5)
legend3d('topright',legend=c('Matheran','Adarwadi'),col=c('#0000ff','#ff0000'),pch=16,cex=1)

 directory <- 'images_for_PCA_gif'
 dir.create(directory)

#Make a series of images (for the GIF)
for (i in 1:360) {
  view3d(userMatrix=rotationMatrix(2*pi * i/360, 0, 1, 0))
  rgl.postscript(filename=paste(directory,"/frame-",
                              sprintf("%03d", i), ".svg", sep=""),fmt='svg')
}
