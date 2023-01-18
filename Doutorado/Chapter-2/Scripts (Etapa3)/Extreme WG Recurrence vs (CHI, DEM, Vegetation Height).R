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

opt_chi = read.csv("Optimal_ERA5_Block_1979_2020.csv", sep = ",")

chi = opt_chi %>%
  group_by(x, y) %>%
  slice(which.max(CHI))
chi = chi[,c(5,6,7)]

#Join data frames -------------------------------------------------------------
dem = dem %>% unite("xy", x:y)
wg = wg %>% unite("xy", x:y)
gedi = gedi %>% unite("xy", x:y)
bioma = bioma %>% unite("xy", x:y)
chi = chi %>% unite("xy", x:y)

df = full_join(dem, wg, id = "xy")
df = full_join(df, gedi, id = "xy")
df = full_join(df, chi, id = "xy")
df = full_join(df, bioma, id = "xy")


df = df %>% na.omit()
df = df[,c(-4)]

df$Frequency = df$Recurrence/41

pont = df %>% filter(xy == "-52.25_-13") #Area1 point

#Regression with quadratic factor for Wind Frequency x DEM -------------------
model = lm(Frequency~DEM+I(DEM^2), data = df)
summary(model)

df$predito=predict(model)

#Correlation plots ------------------------------------------------------------
a = ggplot(df, aes(x=CHI, y=Frequency))+
  geom_point(col = "purple", size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=CHI, y=Frequency), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(col = "black", method = "lm")+
  #scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="CHI Coefficient",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "a) Extreme Wind (1979-2020) x Opitmal CHI")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.8)); a

b = ggplot(df, aes(x=DEM, y=Frequency))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=DEM, y=Frequency), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F)+
  geom_smooth(aes(x=DEM, y=predito), col = "black")+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  labs(x="Elevation Above Sea Level (m)",
       y="Frequency/year of Wind Gust (>10 m/s)",
       title = "b) Extreme Wind (1979-2020) x Elevation Model")+
  #theme_minimal()+
  theme(legend.position = c(0.85, 0.2)); b


c = ggplot(df, aes(x=Frequency, y=Higth))+
  geom_point(aes(col = Biome), size = 4, alpha = 0.7)+
  geom_point(data = pont, aes(x=Frequency, y=Higth), col = "red", alpha = 0.7,
             shape = 24, size = 3, stroke = 2)+
  stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.7)+
  stat_cor(aes(col = Biome), show.legend = F, label.y.npc = 0.85, label.x.npc = 0.7)+
  #facet_wrap(~Biome)+
  scale_colour_manual(values = c("darkgreen", "darkorange"))+
  geom_smooth(method = "lm", col = "black")+
  labs(x="Frequency/year of Wind Gust (>10 m/s)",
       y="Vegetation Height (m)",
       title = "c) Forest Height (2019) x Extreme Wind (1979-2020)")+
  #theme_minimal()+
  theme(legend.position = c(0.09, 0.15)); c


ggsave(filename = "Extreme Wind (1979-2020) x CHI.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)

ggsave(filename = "Extreme Wind (1979-2020) x Elevation Model.png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 16, height = 10, units = "cm", dpi = 300)

ggsave(filename = "Forest Height x Extreme Wind (1979-2020).png", plot = c,
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






















