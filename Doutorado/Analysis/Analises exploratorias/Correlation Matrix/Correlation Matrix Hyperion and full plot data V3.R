#Correlation Matrix Hyperion and full plot data
#By: Eduardo Q Marques 11-05-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
#hy = hy[,c(-5, -9)] #NDWI have so high values in comparison with other indexs
hy$year = as.character(hy$year)

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
df = df[,c(-1)]

#Correlation by ggally
ggcorr(df, geom = "circle", nbreaks = 8)



#Function to join and plot
plotcor = function(x){
  x = x %>% 
    group_by(id) %>% 
    summarise(index = median(index)) 
  
  x = full_join(area, x, by="id")
  x = na.omit(x)
  x = as.numeric(x)
  
  
  
  #eqm = c("#FC4E07","#00AFBB") #Pallete colors(Orange and Blue)
  
  #a = ggplot(x, aes(x=value, y=index, col = variable))+
   # geom_point(size=3, alpha = 0.3)+
    #geom_smooth(method="lm", se=F)+ 
    #stat_cor(show.legend = F)+
    #theme_minimal()+
    #ggtitle(y)+
    #theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))
  
  #a = ggpar(a, palette = eqm)
}

evi = plotcor(evi)
ndvi = plotcor(ndvi,"NDVI")
vari = plotcor(vari,"VARI")
vig = plotcor(vig,"VIG")
lwvi2 = plotcor(lwvi2,"LWVI2")
msi = plotcor(msi,"MSI")
ndii = plotcor(ndii,"NDII")
ndwi = plotcor(ndwi,"NDWI")
pssr = plotcor(pssr,"PSSR")
psri = plotcor(psri,"PSRI")
sipi = plotcor(sipi,"SIPI")
wbi = plotcor(wbi,"WBI")
pri = plotcor(pri,"PRI")
rendvi = plotcor(rendvi,"RENDVI")
nirv = plotcor(nirv,"NIRV")

#evi;ndvi;vari;vig;lwvi2;msi;ndii;ndwi;pssr;psri;sipi;wbi;pri;rendvi;nirv

struc = ggarrange(evi+rremove("xlab")+rremove("ylab"),
                  ndvi+rremove("xlab")+rremove("ylab"),
                  vari+rremove("xlab")+rremove("ylab"),
                  vig+rremove("xlab")+rremove("ylab"),
                  common.legend = TRUE,
                  legend="bottom",
                  ncol = 2, nrow = 2)
struc

bioc = ggarrange(lwvi2+rremove("xlab")+rremove("ylab"),
                 msi+rremove("xlab")+rremove("ylab"),
                 ndii+rremove("xlab")+rremove("ylab"),
                 ndwi+rremove("xlab")+rremove("ylab"),
                 pssr+rremove("xlab")+rremove("ylab"),
                 psri+rremove("xlab")+rremove("ylab"),
                 sipi+rremove("xlab")+rremove("ylab"),
                 wbi+rremove("xlab")+rremove("ylab"),
                 nirv+rremove("xlab")+rremove("ylab"),
                 common.legend = TRUE,
                 legend="bottom",
                 ncol = 3, nrow = 3)
bioc

phy = ggarrange(pri+rremove("xlab")+rremove("ylab"),
                rendvi+rremove("xlab")+rremove("ylab"),
                common.legend = TRUE,
                legend="bottom",
                ncol = 1, nrow = 2)
phy

