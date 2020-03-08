#Correlation Matrix between Hyperion Indices
#By: Eduardo Q Marques 08-03-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
hy = hy[,c(-5, -9)]
hy$data = as.character(hy$data)


#Vegetation Index Correlation =============================================================
ggcorr(hy, label = TRUE)

#Exclude MSI, NBRI, SIPI, PSRI, VARI, ARI
hy2 = hy[,c(-3, -4, -9, -12, -13, -16)]
ggcorr(hy2, label = TRUE)

#Correlations GGPLOT =======================================================================
eqm = c("#FC4E07","#00AFBB") #Pallete colors(Orange and Blue)


#Structural
struc = hy


ggplot(struc, aes(x=wbi, y=msi))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

ggplot(struc, aes(x=nbri, y=ndii))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

ggplot(struc, aes(x=psri, y=ndvi))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

ggplot(struc, aes(x=sipi, y=ndvi))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

ggplot(struc, aes(x=vari, y=vig))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

ggplot(struc, aes(x=ari, y=evi))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=T)+ 
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))
