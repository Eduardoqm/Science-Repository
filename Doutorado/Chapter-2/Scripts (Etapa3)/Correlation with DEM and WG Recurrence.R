#DEM vs Extreme Wind Gust Recurrence

#Eduardo Q Marques 03-01-2023

library(raster)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)

#Load data ----------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

dem = read.csv("DEM_Xingu.csv")

wg = read.csv("Extreme_WG_Recurrence.csv")


#Join dataframes --------------------------------------------------------------
dem = dem %>% unite("xy", x:y)
wg = wg %>% unite("xy", x:y)

df = full_join(dem, wg, id = "xy")
df = df %>% na.omit()

#Correlation plots ------------------------------------------------------------

a = ggplot(df, aes(x=DEM, y=Recurrence))+
        geom_point()+
        stat_cor(show.legend = F)+
        ggtitle("DEM x Extreme Wind Gust (1979 - 2020)")+
        geom_smooth(); a

ggsave(filename = "Xingu_OptCHI_Veg_resampled_Hight_1979_2020.png", plot = a,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 10, height = 10, units = "cm", dpi = 300)




#Plot CHI Maps
library(rgdal)
library(viridis)
pont = readOGR(dsn = "/home/queirozm/eqm_eth_ufz/Data/Shapes", layer = "Area1_ponto") #Area-1 point
pont2 = as.data.frame(pont)
colnames(pont2) = c("id","x","y")

df_mean2 = df_mean %>% separate(xy, c("x","y"), sep = "_")

c_class = ggplot(df_mean2)+
      geom_raster(aes(x, y, fill = Class))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_manual(values = c('#bdd7e7','#6baed6','#3182bd','#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
      labs(x=NULL, y=NULL, title = "Optimal Windowns")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_classes_1979_2020.png", plot = c_class,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 15, height = 20, units = "cm", dpi = 300)


c_value = ggplot(df_mean2)+
      geom_raster(aes(x, y, fill = CHI))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_viridis(option = "magma", direction = -1, limits=c(0, 0.30), name = "Chi q = 0.9")+
      labs(x=NULL, y=NULL, title = "Optimal CHI")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_values_1979_2020.png", plot = c_value,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 15, height = 20, units = "cm", dpi = 300)