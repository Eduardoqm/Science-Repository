#####################################################
# Boxplot and Models to explore Landsat time series #
#                                                   #
# Eduardo Q Marques 26-11-2020                      #
#####################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(gridExtra)

#Data ========================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Take just year information ==================================================================
df$year = as.character(df$year)
df$year = substr(df$year, 1,4)

#Filter to get regeneration ==================================================================
evi = df %>%
  filter(index == 'evi2')

ndvi = df %>%
  filter(index == 'ndvi')

vig = df %>%
  filter(index == 'vig')

ndii = df %>%
  filter(index == 'ndii')

#BOXPLOTS and Models =========================================================================
eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)

pbox = function(z,w){
  a = ggplot(z, aes(x = year, y = value, fill=treat)) +
    geom_boxplot(alpha = 0.7) +
    ggtitle(w)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))+
    scale_fill_manual(values = eqm)
  
  b = ggplot(z, aes(x = year, y = value, color=treat)) +
    geom_smooth(aes(group=treat), alpha = 0.7) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))+
    scale_color_manual(values = eqm)
  
  grid.arrange(a, b, ncol=1)
}

pbox(evi, "EVI")
pbox(ndvi, "NDVI")
pbox(ndii, "NDII")
pbox(vig, "VIG")








