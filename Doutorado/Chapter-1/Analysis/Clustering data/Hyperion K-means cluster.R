library(stats)
library(cluster) 
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =======================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

#Transform data for analysis ================================================================
df2 = df[c(5,7)]
#df2$value = log(df2$value)

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
treat = df[c(1:11781),]; treat = treat[,c(6,7,8)]

#Join everything
df3 = cbind(treat$parcela,treat$year,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(df3)[1] = "Parcela"; colnames(df3)[2] = "year"

#Indices data padronization
df3 = na.omit(df3)
df4 = df3[,3:16]
df4 = scale(df4)
df5 = cbind(df3[,1:2], df4)
df5 = df5[,c(-2)]
df5 = df5 %>% 
  unite("id", "Parcela", "year", sep = "_")

df6 = na.omit(df5)

#Compute and visualize the distance matrix ==================================================
distance <- get_dist(df5)
distance <- get_dist(df6)
#fviz_dist(distance, gradient = list(low = "blue", mid = "white", high = "red"))

#Computing k-means clustering in R
#k2 <- kmeans(df, centers = 2, nstart = 25)
#str(k2)

#Calculate ideal numebers of cluster (k)
fviz_nbclust(df5, kmeans, method = "silhouette")
fviz_nbclust(df6, kmeans, method = "silhouette")

#Final results vizualization
final <- kmeans(df5, 2, nstart = 25)
print(final)

fviz_cluster(final, data = df)
















