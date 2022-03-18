#====================================================#
#PCA Hyperspectral Indices by treatmant and Edge-Core#
#                                                    #
#Eduardo Q Marques 18-03-2022                        #
#====================================================#

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

#Plot cluster experiment moment
df3$cond[df3$Ano == 2004] <- "Pre-Fire"
df3$cond[df3$Ano == 2005] <- "Fire"
df3$cond[df3$Ano == 2006] <- "Fire"
df3$cond[df3$Ano == 2008] <- "Fire"
df3$cond[df3$Ano == 2010] <- "Fire"
df3$cond[df3$Ano == 2011] <- "Fire"
df3$cond[df3$Ano == 2012] <- "Post-Fire"

#Edge - Core separation ===================================================================
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

#PCA Analysis =============================================================================#Select numeric data
df3 = na.omit(df3)
df4 = df3[,c(-1, -2, -3, -21, -22, -23)]

#Running PCA
df4_pca = PCA(df4, graph = T)
fviz_pca_var(df4_pca, col.var = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07", "#FF0000"), 
             repel = TRUE) # Avoid text overlapping

#Plot PCAs by momentum --------------------------------------------------------------------
ggpca = function(w,z){
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

#Treatment
all_plot =  (ggpca(df4_pca, df3))+ylim(-5.5, 10)+xlim(-22,11)
all_plot












library(ggforce)
library(concaveman)

ggpca2 = function(w,z){
  w_gg = as.data.frame(w$ind$coord)
  w_gg$Parcela = z$Parcela
  w_gg$Dist = z$dist2
  w_gg$cond = as.factor(z$cond)
  
  coord = as.data.frame(w$var$coord)
  coord$Dim.1 = coord$Dim.1*10
  coord$Dim.2 = coord$Dim.2*10
  
  ggplot()+
    geom_point(data=w_gg, aes(x=Dim.1, y=Dim.2, shape = Dist, col = Parcela),
               alpha = 0.3, size = 2)+
    stat_ellipse(data=w_gg, aes(x=Dim.1, y=Dim.2, linetype = cond), type = "norm", col = "darkgreen", size = 1)+
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

ggpca2(df4_pca, df3)+ylim(-5.5, 10)+xlim(-22,11)
