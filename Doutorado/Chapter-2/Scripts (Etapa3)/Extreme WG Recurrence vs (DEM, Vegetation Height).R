#Extreme Wind Gust Recurrence vs (DEM, Vegetation Height)

#Bonus: Map of DEM

#Eduardo Q Marques 03-01-2023

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(viridis)
library(rgdal)
library(sf)

#Load data --------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

dem = read.csv("DEM_Xingu.csv")

wg = read.csv("Extreme_WG_Recurrence.csv")

gedi = read.csv("GEDI_mean_pixel.csv", sep = ",")
colnames(gedi) = c("id", "Higth", "x", "y")

bioma = read.csv("Biome_coord.csv", sep = ",")
colnames(bioma) = c("x", "y", "Biome")


#Join data frames -------------------------------------------------------------
dem = dem %>% unite("xy", x:y)
wg = wg %>% unite("xy", x:y)
gedi = gedi %>% unite("xy", x:y)
bioma = bioma %>% unite("xy", x:y)

df = full_join(dem, wg, id = "xy")
df = full_join(df, gedi, id = "xy")
df = full_join(df, bioma, id = "xy")

df = df %>% na.omit()
df = df[,c(-4)]

pont = df %>% filter(xy == "-52.25_-13") #Area1 point

#Correlation plots ------------------------------------------------------------
a = ggplot(df, aes(x=DEM, y=Recurrence))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=DEM, y=Recurrence), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(col = "black")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="Elevation Above Sea Level (m)",
       y="Recurrence of Wind Gust (>10 m/s)",
       title = "a) Extreme Wind (1979-2020) x Elevation Model")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.2)); a


b = ggplot(df, aes(x=Recurrence, y=Higth))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=Recurrence, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.4)+
  facet_wrap(~Biome)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  geom_smooth(method = "lm", col = "black")+
  labs(x="Recurrence of Wind Gust (>10 m/s)",
       y="Vegetation Height (m)",
       title = "b) Forest Height (2019) x Extreme Wind (1979-2020)")+
  #theme_minimal()+
  theme(legend.position = "none"); b


ggsave(filename = "Extreme Wind (1979-2020) x Elevation Model.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)

ggsave(filename = "Forest Height x Extreme Wind (1979-2020).png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)


#Map of Digital Elevation Model ----------------------------------------------------------
#Shapes
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")

xingu <- readOGR("Shapes/Xingu_MT.shp")
xingu = st_as_sf(xingu)

bio = readOGR("Shapes/Limit_bioma_xingu.shp")
bio = st_as_sf(bio)


#Optimal Chi Coefficient Map ---------------------------------------------------
#Area 1 point
df2 = df %>% 
  separate(xy, c("x","y"), sep = "_")

df2$x = as.numeric(df2$x)
df2$y = as.numeric(df2$y)

pont = df2 %>% filter(x == -52.25) %>% filter(y == -13)
#Small adjusts
pont$x = pont$x + 0.125
pont$y = pont$y - 0.125


#The Mao ------------------------------------------------------------------------
eqm = c("white","gray","brown","orange","green","darkgreen")

map = ggplot(df2)+
  geom_raster(aes(x, y, fill = DEM))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "Digital Elevation Model")+
  scale_fill_gradientn(colours = eqm, limits=c(155, 655), name = "Meters")+
  theme_minimal()+
  theme(text = element_text(size = 14)); map

ggsave(filename = "Xingu_Digital Elevation Model.png", plot = map,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)






















