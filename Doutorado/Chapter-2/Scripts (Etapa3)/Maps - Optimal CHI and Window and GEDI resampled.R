###########################################
# Maps - Optimal CHI coefficient,         #
#        Optimal Temporal/Spatial Window, #
#        GEDI Vegetation Higth            #
#                                         #
# Eduardo Q Marques 12-10-2022            #
###########################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

library(tidyverse)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)
library(rgdal)
library(sf)
library(plotly)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

gedi = read.csv("GEDI_mean_pixel.csv", sep = ",")
colnames(gedi) = c("id", "Higth", "x", "y")

fire = read.csv("Fire_frequence_mean_pixel.csv", sep = ",")
colnames(fire) = c("id", "Fire", "x", "y")

opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv", sep = ",")

opt_chi2 = opt_chi %>%
  group_by(x, y) %>%
  slice(which.max(CHI))%>%
  ungroup()

opt_chi2$Class = as.character(opt_chi2$Class)
opt_chi2$Class[opt_chi2$Class == "1d, 0.25Â°"] = c("1d, 0.25°")
opt_chi2$Class[opt_chi2$Class == "1d, 0.75Â°"] = c("1d, 0.75°")
opt_chi2$Class[opt_chi2$Class == "1d, 1.25Â°"] = c("1d, 1.25°")
opt_chi2$Class[opt_chi2$Class == "1d, 1.75Â°"] = c("1d, 1.75°")

opt_chi2$Class[opt_chi2$Class == "3d, 0.25Â°"] = c("3d, 0.25°")
opt_chi2$Class[opt_chi2$Class == "3d, 0.75Â°"] = c("3d, 0.75°")
opt_chi2$Class[opt_chi2$Class == "3d, 1.25Â°"] = c("3d, 1.25°")
opt_chi2$Class[opt_chi2$Class == "3d, 1.75Â°"] = c("3d, 1.75°")

opt_chi2$Class[opt_chi2$Class == "5d, 0.25Â°"] = c("5d, 0.25°")
opt_chi2$Class[opt_chi2$Class == "5d, 0.75Â°"] = c("5d, 0.75°")
opt_chi2$Class[opt_chi2$Class == "5d, 1.25Â°"] = c("5d, 1.25°")
opt_chi2$Class[opt_chi2$Class == "5d, 1.75Â°"] = c("5d, 1.75°")

#Shapes
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")

xingu <- readOGR("Shapes/Xingu_MT.shp")
xingu = st_as_sf(xingu)

bio = readOGR("Shapes/Limit_bioma_xingu.shp")
bio = st_as_sf(bio)


#Optimal Chi Coefficient Map ---------------------------------------------------
#Area 1 point
pont = opt_chi2 %>%
  filter(x == -52.25) %>%
  filter(y == -13)
#Small adjusts
pont$x = pont$x + 0.125
pont$y = pont$y - 0.125


a = ggplot(opt_chi2)+
  geom_raster(aes(x, y, fill = CHI))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "a) Optimal Chi Coefficient")+
  #scale_fill_viridis(option = "plasma", direction = -1, limits=c(0, 0.27943), name = "Chi q = 0.9")+
  scale_fill_gradient(low = "#f7fcb9", high = "blue",
                      limits=c(0.0, 0.27943), name = "Chi q = 0.9")+
  theme_minimal()+
  theme(text = element_text(size = 14)); a

#ggplotly(a)

#Optimal Temporal/Spatial Windowns Map -----------------------------------------
b = ggplot(opt_chi2)+
  geom_raster(aes(x, y, fill = Class))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  scale_fill_manual(values = c('#bdd7e7','#6baed6','#3182bd','#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
  labs(x=NULL, y=NULL, title = "b) Optimal Temporal-Spatial Window")+
  theme_minimal()+
  theme(text = element_text(size = 14)); b


#GEDI Vegetation Higth ---------------------------------------------------------
c = ggplot(gedi)+
  geom_raster(aes(x, y, fill = Higth))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "Forest Crown Height")+
  #scale_fill_viridis(option = "mako", direction = -1, limits=c(0, 30), name = "Height (m)")+
  scale_fill_gradient(low = "#f7fcb9", high = "darkgreen",
                       limits=c(0, 25), name = "Height (m)")+
  theme_minimal()+
  theme(text = element_text(size = 14)); c

#Fire Frequence ----------------------------------------------------------------
d = ggplot(fire)+
  geom_raster(aes(x, y, fill = Fire))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "Fire Frequence Mean (1985-2020)")+
  #scale_fill_viridis(option = "plasma", direction = -1, limits=c(0, 0.27943), name = "Chi q = 0.9")+
  scale_fill_gradient(low = "#f7fcb9", high = "#bd0026",
                      limits=c(0, 35), name = "Frequence")+
  theme_minimal()+
  theme(text = element_text(size = 14)); d


ggsave(filename = "Xingu_Optimal_CHIMap_1979_2020.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)

ggsave(filename = "Xingu_Optimal_Window_1979_2020.png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)

ggsave(filename = "Xingu_Vegetation_Height_2019.png", plot = c,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)


ggsave(filename = "Xingu_Fire_freq_1985_2020.png", plot = d,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)


#Limits of Xingu ----------------------------------------------------------------
e = ggplot(fire)+
  #geom_raster(aes(x, y, fill = Fire))+
  geom_point(data = pont, aes(x, y), col = "red", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "darkgreen", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = NULL)+
  #scale_fill_viridis(option = "plasma", direction = -1, limits=c(0, 0.27943), name = "Chi q = 0.9")+
  theme_minimal()+
  theme(text = element_text(size = 14)); e


ggsave(filename = "Xingu_Limits.png", plot = e,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)





