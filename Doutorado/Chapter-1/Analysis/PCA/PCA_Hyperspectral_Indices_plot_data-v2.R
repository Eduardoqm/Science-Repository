#===============================================#
# PCA Hyperspectral Indices and plot level data #
#                                               #
# Eduardo Q Marques 12-10-2020                  #
#===============================================#


library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

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
treat = df[c(1:11781),]; treat = treat[,c(7,8)]

#Join everything
df3 = cbind(treat$parcela,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(df3)[1] = "parcela"


#Plot level data
#Load data ==========================================================================
lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)
#lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)
#litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)
#fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)


#biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)
#biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

#Data organization =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
lai = lai %>% 
  filter(year <=2012)
lai = lai[,c(1,3)]


litt = litt %>% 
  filter(year <=2012)
litt = litt[,c(1,4)]

#biomass = biomass %>% 
#  filter(data <=2012)



#Join with field data
plot_level = full_join(lai, litt, by = "parcela")

dfx = full_join(plot_level, df3, by = "parcela")





lai = lai[,c(-2)]
lai$id = as.character(lai$id)
lai$lai = lai$lai/10
colnames(lai) = c("id", "LAI")
lai2 = melt(lai)

litt = litt[,c(-2)]
litt$id = as.character(litt$id)
colnames(litt) = c("id", "Litterfall")
litt2 = melt(litt)

area = full_join(lai2, litt2, by = "id")
area = area[,c(-2,-4)]
colnames(area) = c('id', 'LAI', 'Litterfall')

df = full_join(area, indexs, by = "id")

df = df %>% 
  separate(id, c('plot','year'), sep = '_')

control = df %>% 
  filter(plot == "control")
control = control[,c(-1,-2)]

b3yr = df %>% 
  filter(plot == "b3yr")
b3yr = b3yr[,c(-1,-2)]

b1yr = df %>% 
  filter(plot == "b1yr")
b1yr = b1yr[,c(-1,-2)]



#PCA Analysis =====================================================================================
#Tutorial video
#Select numeric data
df3 = na.omit(df3)
df4 = df3[,c(-1)]

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

#Plot cluster treatmant
grp = as.factor(df3[,c(1)])

fviz_pca_biplot(df4_pca, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices")


