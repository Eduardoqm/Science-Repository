###########################################################
# Historical annual accumulated Precipitation (1979-2020) #
#                                                         #
# Eduardo Q Marques 31-10-2023                            #
###########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
#library(rgdal)
library(terra)
library(sf)
library(plotly)


#Load data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data")
prec = rast("Prec_acumulada_ERA5_Land.tif")
sample = rast("SRTM/SRTM_025_xingu_DEM.tif")
plot(sample)

prec = resample(prec, sample)

prec = prec*1000 #To convert m to mm
prec = prec/41 #To make the annual mean

plot(prec)

#Shapes
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")
xingu <- vect("Shapes/Xingu_MT.shp")
plot(xingu, add = T)
xingu = st_as_sf(xingu)

bio = vect("Shapes/Limit_bioma_xingu.shp")
plot(bio, add = T)
bio = st_as_sf(bio)

#Converting precipitation raster to data frame ---------------------------------
df = as.data.frame(prec, xy = T)
colnames(df) = c("x","y","Precipitation")

#Wind Speed at 0.9 quantile ----------------------------------------------------
dens = ggplot(df, aes(x = Precipitation))+
  geom_density(fill = "RoyalBlue", alpha = 0.5)+
  labs(y = NULL, x = "Accumulated Precipitation (mm)")+
  theme_bw();dens

#ggsave(filename = "Extreme_WG_minimal_hist_1979_2020.png", plot = dens,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
#       width = 16, height = 10, units = "cm", dpi = 300)

#Map of annual accumulated Precipitation ---------------------------------------
df$x = as.numeric(df$x)
df$y = as.numeric(df$y)

#Area 1 point
pont = df %>%
  filter(x == -52.14720) %>%
  filter(y == -13.07049)
#Small adjusts
pont$x = pont$x + 0.125
pont$y = pont$y - 0.125


a = ggplot(df)+
  geom_raster(aes(x, y, fill = Precipitation))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "white", fill = NA, size = 1, stroke = 2)+
  #geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "x) Historical Precipition")+
  scale_fill_viridis(option = "mako",
                     direction = -1, limits=c(1300, 2400),
                     name = "Precipitation (mm)")+
  theme_minimal()+
  theme(text = element_text(size = 14)); a



ggsave(filename = "Xingu_Historical_Precipition_1979_2020.png", plot = a,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 12, units = "cm", dpi = 300)
       #width = 20, height = 16, units = "cm", dpi = 300)

setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
write.csv(df, "Accumulated_Precipitation_ERA5L.csv", sep = ",", row.names = F)












