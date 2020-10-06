#=============================#
#PCA Hyperspectral Indices    #
#                             #
#Eduardo Q Marques 05-10-2020 #
#=============================#


library(stats)
library(tidyverse)
library(reshape2)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

#Transform data for analysis ======================================================================
df2 = df[c(5,7)]
df2$value = log(df2$value)

#Transpor variables in columns
transp = function(x){
  z = df2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

ari = transp("ari");colnames(ari) = c("ari")
evi = transp("evi2");colnames(evi) = c("evi")
ndvi = transp("ndvi");colnames(ndvi) = c("ndvi")
vari = transp("vari");colnames(vari) = c("vari")
vig = transp("vig");colnames(vig) = c("vig")
lwvi2 = transp("lwvi2");colnames(lwvi2) = c("lwvi2")
msi = transp("msi");colnames(msi) = c("msi")
ndii = transp("ndii");colnames(ndii) = c("ndii")
ndwi = transp("ndwi");colnames(ndwi) = c("ndwi")
pssr = transp("pssr");colnames(pssr) = c("pssr")
psri = transp("psri");colnames(psri) = c("psri")
sipi = transp("sipi");colnames(sipi) = c("sipi")
wbi = transp("wbi");colnames(wbi) = c("wbi")
pri = transp("pri");colnames(pri) = c("pri")
rendvi = transp("rendvi");colnames(rendvi) = c("rendvi")
nirv = transp("nirv"); colnames(nirv) = c("nirv")

#Stract treatmant name
treat = df[c(1:11781),]; treat = treat[,c(7,8)]

#Join everything
df3 = cbind(treat$parcela,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(df3)[1] = "Parcela"


#PCA Analysis =====================================================================================
df4 = na.omit(df3)
df4 = df4[,c(-1)]
df4_pca = prcomp(df4, center = T)

print(df4_pca)
plot(df4_pca)









## === Transform the data ====
log.trans <- function(x){log(x+1)}   #Perform a logarithm transformation on the data
crop.cover.log <- apply(crop.cover, 2, log.trans)

## === Center the data ===
crop.cover.rowmeans <-rowMeans(crop.cover.log)   # Obtain the mean of each row (or site)
crop.cover.rowmeans.matrix <- matrix(rep(crop.cover.rowmeans, dim(crop.cover.log)[2]), nrow=dim(crop.cover.log)[1])  #Create a matrix with the means for each row
crop.cover.centered <- crop.cover.log-crop.cover.rowmeans.matrix  #Use the created matrix to center the data (each value is substracted the row mean)

## === Check for correlations and for removals ===
crop.cover.means <- colMeans(crop.cover.centered)  # Obtain the column means (or cover means)
crop.cover.means
str(crop.cover.means)		# Examine object to plan subsetting.
crop.cover.common <- subset(crop.cover.centered, select=colMeans(crop.cover.centered)>-2) #Cut off rare species, -2 is the threshold I chose
num.elim <- dim(crop.cover.centered)[2] - dim(crop.cover.common)[2]
num.elim	# See how many uncommon land cover types we eliminated.	
pairs(crop.cover.common[,1:5])  # Analyze correlations between the variables (or land cover types)
pairs(crop.cover.common[,6:10])		
pairs(crop.cover.common[,11:15])	
pairs(crop.cover.common[,16:20])
crop.cover.common <- crop.cover.common[,-2]	 # Remove Soybeans land cover as its correlation with corn is almost perfect

## === Conduct the PCA ===
crop.cover.pca <- prcomp(crop.cover.common, cor=FALSE) #Perform the PCA using the covariance matrix
summary(crop.cover.pca)                                                                   # Explore the results
str(crop.cover.pca)

## === Screeplot to choose the number of PCs ===
plot(crop.cover.pca)                                                                      
screeplot(crop.cover.pca, bstick=T, main="Crop cover PCA")	# Scree plot and broken stick to decide how many PCA axes we want to keep
text(9.8, 45, "Keep PC1, PC2 and PC3")	          

## === Biplots with selected PCs ===
par(mfrow=c(1,3))  # Biplots using the three PCA aces we decided to keep
biplot(crop.cover.pca, choices=1:2)
biplot(crop.cover.pca, choices=c(1,3))
biplot(crop.cover.pca, choices=2:3)

## === Scores for PC1-PC3 ===
crop.cover.pca$x[1:205,1:3]  # Scores corresponding to the 3 PCA axes we decided to keep














