###########################################
# Maps - CHI coefficient by windows       #
#                                         #
# Eduardo Q Marques 21-12-2022            #
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

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

p1 = read.csv("ERA5_Block_1pixel_1979_2020.csv", sep = ",")
p3 = read.csv("ERA5_Block_3pixel_1979_2020.csv", sep = ",")
p5 = read.csv("ERA5_Block_5pixel_1979_2020.csv", sep = ",")
p7 = read.csv("ERA5_Block_7pixel_1979_2020.csv", sep = ",")

opt_chi = rbind(p1, p3, p5, p7)

#Optimal Chi Coefficient Map ---------------------------------------------------
opt_chi = opt_chi %>%
  group_by(xy, window, degrees) %>%
  slice(which.max(CHI)) %>%
  ungroup()

opt_chi$degrees = as.character(opt_chi$degrees)
#opt_chi$degrees[opt_chi$degrees == "0.25°"] = c("0.25?")
#opt_chi$degrees[opt_chi$degrees == "0.75°"] = c("0.75?")
#opt_chi$degrees[opt_chi$degrees == "1.25°"] = c("1.25?")
#opt_chi$degrees[opt_chi$degrees == "1.75°"] = c("1.75?")

#optx = opt_chi[,c(-1,-2)]
#write.csv(optx, "ERA5_Block_pixel_window.csv", row.names = F)

#Shapes
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")

xingu <- readOGR("Shapes/Xingu_MT.shp")
xingu = st_as_sf(xingu)

bio = readOGR("Shapes/Limit_bioma_xingu.shp")
bio = st_as_sf(bio)

#Different surronding Chi Coefficient Map ------------------------------------------
opt_chi2 = opt_chi %>% 
  separate(xy, c("x", "y"), sep = "_")
opt_chi2$x = as.numeric(opt_chi2$x)
opt_chi2$y = as.numeric(opt_chi2$y)

d1 = opt_chi2 %>% 
  filter(window == "1d")

d3 = opt_chi2 %>% 
  filter(window == "3d")

d5 = opt_chi2 %>% 
  filter(window == "5d")

#Area 1 point
pont = d1 %>%
  filter(x == -52.25) %>%
  filter(y == -13)
#Small adjusts
pont$x = pont$x + 0.125
pont$y = pont$y - 0.125


#Chi Map Function
mapplot = function(data, titl){
  ggplot(data)+
    geom_raster(aes(x, y, fill = CHI))+
    geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
    geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
    geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
    coord_sf()+
    labs(x=NULL, y=NULL, title = titl)+
    scale_fill_gradient(low = "#f7fcb9", high = "blue",
                        limits=c(0.0, 0.27943), name = "Chi q = 0.9")+
    facet_wrap(~degrees, nrow = 1)+
    theme_minimal()+
    theme(text = element_text(size = 14))
}


a = mapplot(d1, "a) One day window"); a
b = mapplot(d3, "b) Three days window"); b
c = mapplot(d5, "c) Five days window"); c

ggsave(filename = "1d_CHIMap_1979_2020.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 30, height = 15, units = "cm", dpi = 300)

ggsave(filename = "3d_CHIMap_1979_2020.png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 30, height = 15, units = "cm", dpi = 300)

ggsave(filename = "5d_CHIMap_1979_2020.png", plot = c,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 30, height = 15, units = "cm", dpi = 300)




library(plotly)

ggplotly(a)
ggplotly(c)


















