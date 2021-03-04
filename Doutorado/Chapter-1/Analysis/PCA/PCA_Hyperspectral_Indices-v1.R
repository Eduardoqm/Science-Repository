#=============================#
#PCA Hyperspectral Indices    #
#                             #
#Eduardo Q Marques 05-10-2020 #
#Update in 01-03-2021         #
#=============================#


library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

#Transform data for analysis ======================================================================
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
df3 = cbind(treat$year, treat$parcela,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(df3)[1:2] = c("Ano", "Parcela")


#PCA Analysis =====================================================================================
#Tutorial video
#Select numeric data
df3 = na.omit(df3)
df4 = df3[,c(-1, -2)]

#Standart scale, but you dont need this step if use FactorMiner to do PCA
#df4 = scale(df4)

#Running PCA
df4_pca = PCA(df4, graph = F)

#Extract variance values
eig_val = get_eigenvalue(df4_pca)
eig_val

#Plot each variable proportion of variance
fviz_eig(df4_pca, addlabels = T, ylim = c(0,90))

#Extract data to plot PCA =========================================================================
var = get_pca_var(df4_pca)
ind = get_pca_ind(df4_pca)

#Plot PCA variables
fviz_pca_var(df4_pca, col.var = "red")

#Plot cluster treatmant -------------------------------------------------------------------
grp = as.factor(df3[,c(2)])

fviz_pca_biplot(df4_pca, habillage = grp,
                #addEllipses=TRUE,
                #ellipse.level=0.99,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by plots")+
  scale_color_manual(values=c("orange", "red", "blue"))

#Plot cluster year ------------------------------------------------------------------------
grp = as.factor(df3[,c(1)])

fviz_pca_biplot(df4_pca, habillage = grp,
                #addEllipses=TRUE,
                #ellipse.level=0.99,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by years")

#Plot cluster experiment moment -----------------------------------------------------------
df3$cond[df3$Ano == 2004] <- "Pre-Fire"
df3$cond[df3$Ano == 2005] <- "Fire"
df3$cond[df3$Ano == 2006] <- "Fire"
df3$cond[df3$Ano == 2008] <- "Fire"
df3$cond[df3$Ano == 2010] <- "Fire"
df3$cond[df3$Ano == 2011] <- "Fire"
df3$cond[df3$Ano == 2012] <- "Post-Fire"

grp = as.factor(df3[,c(17)])

fviz_pca_biplot(df4_pca, habillage = grp,
                #addEllipses=TRUE,
                #ellipse.level=0.99,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by experiment moment")+
  scale_color_manual(values=c("red", "orange", "darkgreen"))









