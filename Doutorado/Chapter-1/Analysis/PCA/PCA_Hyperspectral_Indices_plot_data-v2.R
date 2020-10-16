#===============================================#
# PCA Hyperspectral Indices and plot level data #   FAIL!!!
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
treat = df[c(1:11781),]; treat = treat[,c(6,7,8)]

treat = unite(treat, "id", c("parcela", "year"), sep = "_") 

#Join every indice
df3 = cbind(treat$id,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(df3)[1] = "id"


#Plot level data
#Load data ==========================================================================
lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)

#biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)

#Data organization =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
lai = lai %>% 
  filter(year %in% c(2004, 2005, 2006, 2008, 2010, 2011, 2012)) %>% 
  unite("trans_line", c("transecto", "linhas"), sep = "") %>% 
  unite("id", c("parcela", "year", "trans_line"), sep = "_") 
lai = lai[,c(1,2)]

litt = litt %>% 
  filter(year %in% c(2004, 2005, 2006, 2008, 2010, 2011, 2012)) %>% 
  unite("id", c("parcela", "year", "trans_line"), sep = "_")
litt = litt[,c(1,4)]

#biomass = biomass %>% 
#  filter(data <=2012)


#Join with field data
plot_level = full_join(lai, litt, by = "id")
plot_level = na.omit(plot_level)
#[ reached 'max' / getOption("max.print") -- omitted 9830 rows ] With NAs
#[ reached 'max' / getOption("max.print") -- omitted 3334 rows ] After omit NAs

plot_level = plot_level %>% 
  separate("id", c("parcela", "year", "trans_line"), sep = "_") %>% 
  unite("id", c("parcela", "year"), sep = "_")
plot_level = plot_level[,c(-2)]
  
dfx = full_join(plot_level, df3, by = "id")
dfx = na.omit(dfx)
#[ reached 'max' / getOption("max.print") -- omitted 2058812 rows ] With NAs
#[ reached 'max' / getOption("max.print") -- omitted 1996328 rows ] After omit NAs

dfx = dfx %>% 
  separate("id", c("parcela", "year"), sep = "_")


#PCA Analysis =====================================================================================
#Select numeric data
#df3 = na.omit(df3)
df4 = dfx[,c(-1,-2)]

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
grp = as.factor(dfx[,c(1)])
grp2 = as.factor(dfx[,c(2)])


fviz_pca_biplot(df4_pca, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by plot")

fviz_pca_biplot(df4_pca, habillage = grp2,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by year")

















#Another alternative
#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
#hy = hy[,c(-5, -9)] #NDWI have so high values in comparison with other indexs
hy$year = as.character(hy$year)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1/Old data")


#biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)
biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

#lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)
lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

#litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)
litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)
fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)

#Data organization =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai$data = as.numeric(lai$data)
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(data <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(biomass) = c("id", "biomass", "dist")

lai = lai %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(litt) = c("id", "dist", "litter")

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(fuel) = c("id", "fuel", "dist")


#Transpor Hyperion data
hy2 = hy[,c(4,5,6)]
#hy2$id = as.character(hy2$id)

geti = function(x){
  z = hy2 %>% 
    na.omit() %>% 
    filter(index == x) %>% 
    group_by(id) %>% 
    summarise(value = median(value))
}

ari = geti("ari");colnames(ari) = c("id","ari")
evi = geti("evi2");colnames(evi) = c("id","evi")
ndvi = geti("ndvi");colnames(ndvi) = c("id","ndvi")
vari = geti("vari");colnames(vari) = c("id","vari")
vig = geti("vig");colnames(vig) = c("id","vig")
lwvi2 = geti("lwvi2");colnames(lwvi2) = c("id","lwvi2")
msi = geti("msi");colnames(msi) = c("id","msi")
ndii = geti("ndii");colnames(ndii) = c("id","ndii")
ndwi = geti("ndwi");colnames(ndwi) = c("id","ndwi")
pssr = geti("pssr");colnames(pssr) = c("id","pssr")
psri = geti("psri");colnames(psri) = c("id","psri")
sipi = geti("sipi");colnames(sipi) = c("id","sipi")
wbi = geti("wbi");colnames(wbi) = c("id","wbi")
pri = geti("pri");colnames(pri) = c("id","pri")
rendvi = geti("rendvi");colnames(rendvi) = c("id","rendvi")
nirv = geti("nirv"); colnames(nirv) = c("id","nirv")

indexs = cbind(evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
indexs = indexs[,c(1,2,4,8,10,12,14,16,18,20,22,24,26,28)]

#Join with field data
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


library(stats)
library(FactoMineR)
library(factoextra)
#PCA Analysis =====================================================================================
#Select numeric data
#df3 = na.omit(df3)
df3 = na.omit(df)
df4 = df3[,c(-1,-2)]

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
grp2 = as.factor(df3[,c(2)])


fviz_pca_biplot(df4_pca, habillage = grp,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by plot")

fviz_pca_biplot(df4_pca, habillage = grp2,
                col.var = "black",
                geom.ind = c("point"),
                title = "Hyperspectral Indices by year")



