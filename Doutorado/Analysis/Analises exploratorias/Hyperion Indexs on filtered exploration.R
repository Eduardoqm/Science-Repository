#Cap-1 Hyperion Indexs on filtered exploration
#By: Eduardo Q Marques 20-02-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)

#Boxplots ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
hy = hy[,c(-5, -9)]
hy$data = as.character(hy$data)

#Exclude MSI, NBRI, SIPI, PSRI, VARI, ARI
hy2 = hy[,c(-3, -4, -9, -12, -13, -16)]


df2 = melt(hy2)

eqm = c("#FC4E07","#00AFBB") #Pallete colors(Orange and Blue)

#Control
control = df2 %>% 
  filter(parcela == "controle") %>% 
  filter(dist == "nucleo")


a = ggplot(control, aes(data,value, fill=dist))+ 
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE)+
  #geom_violin()+
  facet_wrap(~variable, scales="free") +
  #facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

control = ggpar(a, palette = eqm)
control

#Burned
burned = df2 %>% 
  filter(parcela != "controle")


b = ggplot(burned, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  facet_wrap(~variable, scales="free") +
  #facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

burned = ggpar(b, palette = eqm)
burned
