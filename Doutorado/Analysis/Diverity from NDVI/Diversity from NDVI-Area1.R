#Rasterdiv basics. Derive indices of diversity from NDVI
#Test run with my data (hyperion)
#Eduardo Q Marques 28-04-2020

library(rasterdiv)
library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)

setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI')

ndvi = raster("LT05_L1TP_224069_20110830_20161007_01_T1_sr_band1.tif")

#Polygon Area1
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Area1")
area1 = spTransform(area1, crs(ndvi))

ndvi = crop(ndvi, area1)
plot(ndvi)

ndvi = reclassify(ndvi, cbind(253, 255, NA), right=TRUE)
plot(ndvi)

#Derive Hill’s and Renyi’s indices
#Shannon's Diversity
sha <- Shannon(ndvi,window=9,np=1,na.tolerance=0.1)

#Pielou's Evenness
pie <- Pielou(ndvi,window=9,np=1,na.tolerance=0.1)

#Berger-Parker
ber <- BergerParker(ndvi,window=9,np=1,na.tolerance=0.1)

#Rao's quadratic Entropy
rao <- Rao(ndvi,window=9,np=1,na.tolerance=0.1,dist_m="euclidean",shannon=FALSE)

#Cumulative Residual Entropy
cre <- CRE(ndvi,window=9,np=1,na.tolerance=0.1)

#Hill's numbers
hil <- Hill(ndvi,window=9,np=1,na.tolerance=0.1,alpha=seq(0,2,0.5))

#Renyi
ren <- Renyi(ndvi,window=9,np=1,na.tolerance=0.1,alpha=seq(0,2,0.5))


#Transform output matrices to RasterLayer
shara <- raster(sha,template=ndvi)
piera <- raster(pie,template=ndvi)
berra <- raster(ber,template=ndvi)
raora <- raster(rao,template=ndvi)
crera <- raster(cre,template=ndvi)
hilra <- lapply(hil, function(x) raster(x,template=ndvi))
renra <- lapply(ren, function(x) raster(x,template=ndvi))



#Shannon's Diversity
levelplot(shara,main="Shannon's entropy (9 px-side moving window)")

#Pielou's Evenness
levelplot(piera,main="Pielou's evenness (9 px-side moving window)")

#Berger-Parker' Index
levelplot(berra,main="Berger-Parker's index (9 px-side moving window)")

#Rao's quadratic Entropy
levelplot(crera,main="Rao's quadratic entropy (9 px-side moving window)")

#Cumulative Residual Entropy
levelplot(crera,main="Cumulative Resiudal Entropy (9 px-side moving window)")

#Hill's numbers (alpha=0, 1, 1.5 and 2)
levelplot(stack(hilra),main="Hill's numbers (9 px-side moving window)")

#Renyi' Index (alpha=0, 1, 1.5 and 2)
levelplot(stack(renra),main="Renyi's entropy (9 px-side moving window)")


#PCA
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(factoextra)

todf = function(x){
  y = melt(as.data.frame(x))
  y = y[,2]
  y = as.data.frame(y)
}

sha2 = todf(sha); colnames(sha2) = c("Shannon")
pie2 = todf(pie); colnames(pie2) = c("Pielou")
ber2 <- todf(ber); colnames(ber2) = c("Berger-Parker")
rao2 <- todf(rao); colnames(rao2) = c("Rao")
cre2 <- todf(cre); colnames(cre2) = c("CRE")
hil2 <- todf(hil); colnames(hil2) = c("Hill")
ren2 <- todf(ren); colnames(ren2) = c("Renyi")

res = cbind(sha2, pie2, ber2, rao2, cre2, hil2, ren2)
res = res %>% na.omit()

pca = prcomp(res, center = TRUE, scale. = TRUE)


#fviz_pca_ind(pca, label="none", habillage=pca$x, addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")


