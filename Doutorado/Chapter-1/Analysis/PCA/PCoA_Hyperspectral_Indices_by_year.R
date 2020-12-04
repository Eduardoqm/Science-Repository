#===========================================================#
# PCoA Hyperspectral Indices  by Years                      #
# and PCoA Hyperspectral and Multispectral Indices Together #                                #                                                           #
#                                                           #
# Eduardo Q Marques 17-11-2020                              #
#===========================================================#

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#For Hyperpectral data ============================================================================
#Transform data for analysis
hy2 = hy[c(5,7)]
#hy2$value = log(hy2$value)

#Transpor variables in columns
transp = function(x){
  z = hy2 %>% 
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
treat = hy[c(1:11781),]; treat = treat[,c(6,7,8)]

#Join everything
hy3 = cbind(treat$parcela,treat$year,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(hy3)[1] = "Parcela"; colnames(hy3)[2] = "year"

#Make data frame by year
getyear = function(x){
  hy3 %>% 
    na.omit() %>% 
    filter(year == x)
}

p2004 = getyear(2004)
p2005 = getyear(2005)
p2006 = getyear(2006)
p2008 = getyear(2008)
p2010 = getyear(2010)
p2011 = getyear(2011)
p2012 = getyear(2012)

#PCoA Analysis
#Create a distance data
p2010b = p2010[,c(-1,-2)]
p2010c = log(p2010b + 1)

library(ape)
library(vegan)
veg = vegdist(p2010c, "bray")
res = pcoa(veg)

view(res$values)
biplot(res)

pcoa2010 = as.data.frame(res$vectors[,c(1,2)])
pcoa2010$treat = p2010$Parcela

ggplot(pcoa2010, aes(x=Axis.1, y=Axis.2, col = treat))+
  geom_point()













