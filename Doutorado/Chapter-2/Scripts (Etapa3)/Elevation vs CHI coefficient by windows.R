###########################################
# Elevation vs CHI coefficient by windows #
#                                         #
# Eduardo Q Marques 30-04-2024            #
###########################################


library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(viridis)
library(sf)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
dir()

chi = read.csv("ERA5_Block_pixel_window.csv", sep = ",")
dem = read.csv("DEM_Xingu.csv")

#Join CHI and Elevation --------------------------------------------------------
dem2 = dem %>% 
  unite("xy", x, y, sep = "_")

df = full_join(chi, dem2, by = "xy")

#Separating by windows of days -------------------------------------------------
d1 = df %>% 
  filter(window == "1d")

d3 = df %>% 
  filter(window == "3d")

d5 = df %>% 
  filter(window == "5d")

pontd1 = d1 %>% filter(xy == "-52.25_-13") #Area1 point
pontd3 = d3 %>% filter(xy == "-52.25_-13") #Area1 point
pontd5 = d5 %>% filter(xy == "-52.25_-13") #Area1 point

#Linear Models -----------------------------------------------------------------
md1p1 = lm(CHI~DEM, data = d1 %>% filter(degrees == "0.25°")); summary(md1p1)


#Graphics ----------------------------------------------------------------------
g1 = ggplot(d1, aes(x=DEM, y=CHI))+
  geom_point()+
  geom_point(data = pontd1, aes(x=DEM, y=CHI), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  facet_wrap(~degrees, nrow = 1)+
  ylim(0, 0.30)+
  labs(x=NULL,
       y=" ",
       title = "a) One day window")


g3 = ggplot(d3, aes(x=DEM, y=CHI))+
  geom_point()+
  geom_point(data = pontd3, aes(x=DEM, y=CHI), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  facet_wrap(~degrees, nrow = 1)+
  ylim(0, 0.30)+
  labs(x=NULL,
       y="CHI Coefficient",
       title = "b) Three days window")

g5 = ggplot(d5, aes(x=DEM, y=CHI))+
  geom_point()+
  geom_point(data = pontd5, aes(x=DEM, y=CHI), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F)+
  facet_wrap(~degrees, nrow = 1)+
  ylim(0, 0.30)+
  labs(x="Elevation (m)",
       y=" ",
       title = "c) Five days window")


gx = ggarrange(g1, g3, g5, ncol = 1)



ggsave(filename = "CHIxElevation.png", plot = gx,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 20, units = "cm", dpi = 300)



