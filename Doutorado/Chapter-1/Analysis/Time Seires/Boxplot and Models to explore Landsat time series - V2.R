#####################################################
# Boxplot and Models to explore Landsat time series #
#                                                   #
# Eduardo Q Marques 26-07-2021                      #
#####################################################
library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.character(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

eqm = c("orange", "red", "blue") #My color palette

ggplot(df, aes(x=year, y=value, color=treat))+
  geom_jitter(alpha = 0.03)+
  geom_boxplot(position=position_dodge(1), outlier.shape = NA)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  facet_grid(rows = vars(index), scales = "free")+
  theme_bw()+
  scale_color_manual(values = eqm)+
  #scale_fill_manual(values = eqm)+
  theme(axis.text.x = element_text(angle = 90))
  #theme(text = element_text(family = "Times New Roman", size = 14))












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








