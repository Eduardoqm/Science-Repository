##########################################
# CHI coefficient threshold (1979-2020)  #
#                                        #
# Eduardo Q Marques 10-10-2022           #
##########################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(extRemes)
library(boot)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

df1p = read.csv("ERA5_Block_1pixel_1979_2020.csv", sep = ",")
df3p = read.csv("ERA5_Block_3pixel_1979_2020.csv", sep = ",")
df5p = read.csv("ERA5_Block_5pixel_1979_2020.csv", sep = ",")
df7p = read.csv("ERA5_Block_7pixel_1979_2020.csv", sep = ",")

df1p$degrees = c("0.25°")
df3p$degrees = c("0.75°")
df5p$degrees = c("1.25°")
df7p$degrees = c("1.75°")

#df1p = df1p %>% separate(xy, c("x","y"), sep = "_")
#df3p = df3p %>% separate(xy, c("x","y"), sep = "_")
#df5p = df5p %>% separate(xy, c("x","y"), sep = "_")
#df7p = df7p %>% separate(xy, c("x","y"), sep = "_")

#Filter data by pixel overlapped with Tanguro farm -----------------------------
df1p2 = df1p %>% filter(xy == "-52.25_-13")
df3p2 = df3p %>% filter(xy == "-52.25_-13")
df5p2 = df5p %>% filter(xy == "-52.25_-13")
df7p2 = df7p %>% filter(xy == "-52.25_-13")

#Correlation by different blocked days -----------------------------------------
corx = function(a, b){
  df = a
  blw12 = df %>% filter(date == "2012-10-25")
  blw19 = df %>% filter(date == "2019-02-02")
  
  ggplot(df, aes(x=Prec, y=Wind))+
    geom_point(aes(col = window), alpha = 0.1, size = 3)+
    #geom_smooth(aes(col = window), method = "lm")+
    geom_point(data=blw12, aes(x=Prec, y=Wind, fill=window), size=4, shape = 24)+
    geom_point(data=blw19, aes(x=Prec, y=Wind, fill=window), size=4, shape = 22)+
    stat_cor(aes(col = window), show.legend = F, label.y.npc="bottom", label.x.npc = 0.4)+
    labs( x = "Precipitação Máxima (mm)", y = "Rajada de Vento (m/s)", title = b, fill = "Janela", col = "Janela", linetype = "Janela")+
    scale_color_manual(values = c("orange", "blue", "red"))+
    scale_fill_manual(values = c("orange", "blue", "red"))+
    ylim(3, 16)+ xlim(0, 31)+
    theme_bw()+
    theme(legend.position = c(0.9, 0.25))
}

a1 = corx(df1p2, "a) 0.25°"); a1
a2 = corx(df3p2, "b) 0.75°"); a2
a3 = corx(df5p2, "c) 1.25°"); a3
a4 = corx(df7p2, "d) 1.75°"); a4

ggsave(filename = "ERA_Corr_1p_1-3-5d_port.png", plot = a1,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_3p_1-3-5d_port.png", plot = a2,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_5p_1-3-5d_port.png", plot = a3,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_7p_1-3-5d_port.png", plot = a4,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

#Calculating the CHI Coefficient Threshold -------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames/CHIs")

chip1p = read.csv("CHI_1p.csv", sep = ",")
chip3p = read.csv("CHI_3p.csv", sep = ",")
chip5p = read.csv("CHI_5p.csv", sep = ",")
chip7p = read.csv("CHI_7p.csv", sep = ",")

#Plot Tail Dependence Threshold ------------------------------------------------
chiplot = function(a, b){
  ggplot(a, aes(quant, value, fill = window, linetype = window))+
    geom_line(aes(col = window))+
    labs(x = "Quantil", y = "Chi", title = b,
         fill = "Janela", col = "Janela", linetype = "Janela")+
    geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
    scale_fill_manual(values = c("orange", "blue", "red"))+
    scale_color_manual(values = c("orange", "blue", "red"))+
    ylim(0, 0.75)+
    theme_bw()+
    theme(legend.position = c(0.85, 0.8))
}

b1 = chiplot(chip1p, "a) 0.25°"); b1
b2 = chiplot(chip3p, "b) 0.75°"); b2
b3 = chiplot(chip5p, "c) 1.25°"); b3
b4 = chiplot(chip7p, "d) 1.75°"); b4


ggsave(filename = "ERA_CHI_1p_1-3-5d_port.png", plot = b1,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_3p_1-3-5d_port.png", plot = b2,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_5p_1-3-5d_port.png", plot = b3,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_7p_1-3-5d_port.png", plot = b4,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

















