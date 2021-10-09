#===================================================#
# PCA Hyperspectral Indices  by Years and Edge-Core #
#                                                   #
# Eduardo Q Marques 09-10-2021                      #
#===================================================#

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(extrafont)
library(ggplot2)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

#df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Transform data for analysis ======================================================================
#df2 = df[c(5,7)]
df2 = df %>% 
  select(index, value)
#df2$value = log(df2$value)

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
nbr = transp("nbr"); colnames(nbr) = c("NBR")
nbr2 = transp("nbr2"); colnames(nbr2) = c("NBR2")


#Stract treatmant name
#treat = df[c(1:11781),]#; treat = treat[,c(6,7,8)]
treat = df[c(1:12019),]
treat = treat %>% 
  select(year, treat, y)


#Join everything
df3 = cbind(treat$year, treat$treat, treat$y,  evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv, lwvi2, nbr, nbr2)
colnames(df3)[1:3] = c("Ano", "Parcela", "y")

#Change names
df3$Parcela = as.character(df3$Parcela)
df3$Parcela[df3$Parcela == "b1yr"] <- "B1yr"
df3$Parcela[df3$Parcela == "b3yr"] <- "B3yr"
df3$Parcela[df3$Parcela == "control"] <- "Controle"

#Edge - Core separation ====================================================================
summary(df3$y)
min(df3$y) #-13.08359 = 1000 meters
max(df3$y) #-13.07419 = 0 meters

diffy = min(df3$y) - max(df3$y)
df3$dist = ((max(df3$y) - df3$y)/diffy)*1000
df3$dist = abs(df3$dist)
summary(df3$dist)

df3$dist2 = c("a")
df3$dist2[df3$dist <= 250] = c("Borda")
df3$dist2[df3$dist > 250] = c("Interior")

#Make data frame by year ======================================
getyear = function(x){
  df3 %>% 
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


#Plot PCAs by years -------------------------------------------
ggpca = function(w){
  z = w
  w = PCA(w[,c(-1, -2, -3, -21, -22)], graph = F)
  w_gg = as.data.frame(w$ind$coord)
  w_gg$Parcela = z$Parcela
  w_gg$Dist = z$dist2
  
  coord = as.data.frame(w$var$coord)
  coord$Dim.1 = coord$Dim.1*10
  coord$Dim.2 = coord$Dim.2*10
  
  ggplot()+
    geom_point(data=w_gg, aes(x=Dim.1, y=Dim.2, shape = Dist, col = Parcela),
               alpha = 0.3, size = 2)+
    geom_segment(data=coord, aes(xend=Dim.1, yend=Dim.2),x=0,y=0,
                 arrow = arrow(length = unit(0.2, "cm")), col = "#525252")+
    geom_text(data=coord, aes(x=Dim.1, y=Dim.2,label=row.names(coord)),vjust=-0.5,size=4)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()+
    xlab(paste0("PC1 ", format(round(w$eig[1,2], 1), nsmall = 1), "%"))+
    ylab(paste0("PC2 ", format(round(w$eig[2,2], 1), nsmall = 1), "%"))+
    scale_color_manual(values=c("orange", "red", "blue"))+
    scale_shape_manual(values = c(17, 19))+
    theme(text = element_text(family = "Times New Roman", size = 14))
}


a = ggpca(p2004)+ggtitle("2004")+ylim(-5, 8)
b = ggpca(p2005)+ggtitle("2005")
c = ggpca(p2006)+ggtitle("2006")+ylim(-5, 8)
d = ggpca(p2008)+ggtitle("2008")+ylim(-7.5, 6)
e = ggpca(p2010)+ggtitle("2010")+ylim(-5, 8)
f = ggpca(p2011)+ggtitle("2011")
g = ggpca(p2012)+ggtitle("2012")+ylim(-5, 9.5)

library(ggpubr)
pca_panel = ggarrange(a,b,c,d,e,f,g,
                      common.legend = TRUE,
                      legend="right",
                      ncol = 4, nrow = 2)

ggsave(filename = "PCA_panel_years.png", plot = pca_panel,
     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 40, height = 20, units = "cm", dpi = 300)



