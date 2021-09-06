#==========================================================#
# PCA Hyperspectral Indices  by Years                      #
# and PCA Hyperspectral and Multispectral Indices Together #                                       
#                                                          #
# Eduardo Q Marques 17-11-2020                             #
#==========================================================#

#OBS: Almost like version 3, add PCAs for both sensor mult and hyper together

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#For Hyperpectral data ============================================================================
#Transform data for analysis
#hy2 = hy[c(5,7)]
#hy2$value = log(hy2$value)
hy2 = hy %>% 
  select(index, value)

#Transpor variables in columns
transp = function(x){
  z = hy2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

ari = transp("ari");colnames(ari) = c("ARI")
evi = transp("evi2");colnames(evi) = c("EVI")
ndvi = transp("ndvi");colnames(ndvi) = c("NDVI")
vari = transp("vari");colnames(vari) = c("VARI")
vig = transp("vig");colnames(vig) = c("VIG")
lwvi2 = transp("lwvi2");colnames(lwvi2) = c("LWVI2")
msi = transp("msi");colnames(msi) = c("MSI")
ndii = transp("ndii");colnames(ndii) = c("NDII")
ndwi = transp("ndwi");colnames(ndwi) = c("NDWI")
pssr = transp("pssr");colnames(pssr) = c("PSSR")
psri = transp("psri");colnames(psri) = c("PSRI")
sipi = transp("sipi");colnames(sipi) = c("SIPI")
wbi = transp("wbi");colnames(wbi) = c("WBI")
pri = transp("pri");colnames(pri) = c("PRI")
rendvi = transp("rendvi");colnames(rendvi) = c("RENDVI")
nirv = transp("nirv"); colnames(nirv) = c("NIRv")
nbr = transp("nbr"); colnames(nbr) = c("NBR")
nbr2 = transp("nbr2"); colnames(nbr2) = c("NBR2")

#Stract treatmant name
treat = hy[c(1:12019),] #; treat = treat[,c(6,7,8)]
treat = treat %>% 
  select(year, treat)

#Join everything
hy3 = cbind(treat$year, treat$treat,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv, lwvi2, nbr, nbr2)
colnames(hy3)[1:2] = c("Ano", "Parcela")

#Change names
hy3$Parcela = as.character(hy3$Parcela)
hy3$Parcela[hy3$Parcela == "b1yr"] <- "B1yr"
hy3$Parcela[hy3$Parcela == "b3yr"] <- "B3yr"
hy3$Parcela[hy3$Parcela == "control"] <- "Controle"

#Removing Outliers =========================================================================
hy3b = hy3 #Copy from original
hy3b$cont = 1

#Function to find, store and remove rows with outliers
outrem = function(x){
  outliers <- boxplot(x, plot=FALSE)$out #Storing outliers into a vector
  z = hy3b[-which(x %in% outliers),] #Remove the rows with the outliers
}

#Looping process
for (w in 3:19) {
  boxplot(hy3b[,c(-1,-2,-20,-21)])
  hy3b = outrem(hy3b[,w])
  print(sum(hy3b$cont))
}

#Comparison
boxplot(hy3[,c(-1,-2,-20)])
boxplot(hy3b[,c(-1,-2,-20,-21)])

hy3 = hy3b[,-20]

#Make data frame by year ===================================================================
getyear = function(x){
  hy3 %>% 
  na.omit() %>% 
  filter(Ano == x)
}

p2004 = getyear(2004)
p2005 = getyear(2005)
p2006 = getyear(2006)
p2008 = getyear(2008)
p2010 = getyear(2010)
p2011 = getyear(2011)
p2012 = getyear(2012)

#PCA Analysis
pcax = function(x){
  y1 = c(x[2,1])
  y2 = c("Hyperspectral Indices")
  name = paste(y2, y1, sep = " ", collapse = NULL)
  z = x[,c(-1,-2,-20)]
  pcaz = PCA(z, graph = F)
  grp = as.factor(x[,c(2)])
  fviz_pca_biplot(pcaz, habillage = grp,pointshape = 19, pointsize = 2,
                  geom.ind = c("point"),
                  col.var = "black", alpha = 0.25,
                  title = name, legend.title = "Parcela")+
    scale_color_manual(values=c( "orange", "red", "blue"))+
    theme(text = element_text(family = "Times New Roman", size = 14))
  
}


pcax(p2004)
pcax(p2005)
pcax(p2006)
pcax(p2008)
pcax(p2010)
pcax(p2011)
pcax(p2012)

#PCA for all years together
hy4 = na.omit(hy3)
hy4b = hy4[,c(-1,-2)]
pcaz = PCA(hy4b, graph = F)
grp = as.factor(hy4[,c(1)])
fviz_pca_biplot(pcaz, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices All Years")
























#It is garbage!
#For Landsat data ============================================================================
#Transform data for analysis
land2 = land[c(6,8)]

#Transpor variables in columns
transp = function(x){
  z = land2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

evi = transp("evi2");colnames(evi) = c("land_evi")
ndvi = transp("ndvi");colnames(ndvi) = c("land_ndvi")
vig = transp("vig");colnames(vig) = c("land_vig")
ndii = transp("ndii");colnames(ndii) = c("land_ndii")

#Stract treatmant name
treat = land[c(1:116688),]; treat = treat[,c(7,8,9)]

#Join everything
land3 = cbind(treat$treat,treat$year,evi,ndvi,vig,ndii)
colnames(land3)[1] = "Parcela"; colnames(land3)[2] = "year"

#PCA for all years together
land4 = na.omit(land3)
land4b = land4[,c(-1,-2)]
pcaz = PCA(land4b, graph = F)
grp = as.factor(land4[,c(1)])
fviz_pca_biplot(pcaz, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Multispectral Indices All Years")

#Match Hyperion and Lansat data ===================================================================
#Match Landsat and Hyperion period
land3$year = substr(land3$year, 1, 4)

land5 = land3 %>% 
  filter(year %in% c(2004, 2005, 2006, 2008, 2010, 2011, 2012)) %>% 
  unite("id", Parcela, year, sep = "_")

hy5 = hy3 %>% 
  unite("id", Parcela, year, sep = "_")

df = full_join(hy5, land5, id = "id")
df2 = df %>% 
  na.omit() %>% 
  separate(id, c("Parcela", "year"), sep = "_")


#PCA for all Sensors

hy3 = df2[,c(-1,-2)]
pcaz = PCA(hy3, graph = F)
grp = as.factor(df2[,c(1)])
fviz_pca_biplot(pcaz, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Mult and Hyper Indices (2004-2012)")




















