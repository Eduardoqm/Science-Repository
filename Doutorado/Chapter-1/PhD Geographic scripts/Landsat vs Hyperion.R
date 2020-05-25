#Landsat vs Hyperion
#By: Eduardo Q Marques 03-03-2020

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

land = read.csv("Landast_indexs_all by plot.csv", sep = ',')
land$data = as.character(land$data)

hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
hy$data = as.character(hy$data)


#Data integration ===================================================================
hy2 = melt(hy)
hy2$sat = c("Hyperion")

hy2 = hy2 %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')


land2 = melt(land)
land2$sat = c("Landsat")

land2 = land2 %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

df2 = rbind(hy2, land2)
df2 = df2 %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#Boxplots ======================================
#Restoration time
eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange and Blue)



land3 = df2 %>% 
  filter(data %in% c(2004:2012)) %>% 
  filter(parcela == "controle") %>% 
  filter(dist == "nucleo") %>% 
  filter(variable %in% c('ndvi','evi','vig'))


a = ggplot(land3, aes(data,value, fill=sat))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  #facet_wrap(~variable, scales="free") +
  facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

a = ggpar(a, palette = eqm)


land4 = df2 %>% 
  filter(data %in% c(2004:2012)) %>% 
  filter(parcela == "controle") %>% 
  filter(dist == "nucleo") %>% 
  filter(variable %in% c('ndwi','ndii','nbri'))

b = ggplot(land4, aes(data,value, fill=sat))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

b = ggpar(b, palette = eqm)

land_time = ggarrange(a, b, common.legend = TRUE, legend="bottom", ncol = 2, nrow = 1)

land_time


