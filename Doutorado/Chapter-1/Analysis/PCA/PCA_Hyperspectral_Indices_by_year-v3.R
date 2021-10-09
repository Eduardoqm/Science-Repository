#=====================================#
# PCA Hyperspectral Indices  by Years #
#                                     #
# Eduardo Q Marques 05-10-2020        #
# Update: 05-03-2021                  #
#=====================================#


library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Transform data for analysis ======================================================================
df2 = df %>% 
  select(index, value)

#Transpor variables in columns
transp = function(x){
  z = df2 %>% 
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

#Stract treatmant name
treat = df[c(1:11781),]#; treat = treat[,c(4,7)]
treat = treat %>% 
  select(year, treat)

#Join everything
df3 = cbind(treat$parcela,treat$year,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)

colnames(df3)[1] = "Parcela"; colnames(df3)[2] = "year"

#Change names
df3$Parcela = as.character(df3$Parcela)
df3$Parcela[df3$Parcela == "b1yr"] <- "B1yr"
df3$Parcela[df3$Parcela == "b3yr"] <- "B3yr"
df3$Parcela[df3$Parcela == "control"] <- "Controle"

#Make data frame by year ==========================================================================
getyear = function(x){
  df3 %>% 
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

#PCA Analysis =====================================================================================
pcax = function(x){
  y1 = c(x[2,2])
  y2 = c("Hyperspectral Indices")
  name = paste(y2, y1, sep = " ", collapse = NULL)
  z = x[,c(-1,-2)]
  pcaz = PCA(z, graph = F)
  grp = as.factor(x[,c(1)])
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
pcax(p2011)#+ylim (-5, 6)
pcax(p2012)






