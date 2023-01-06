############################################
# Wind Speed Recurrence Count (1979-2020)  #
#                                          #
# Eduardo Q Marques 02-01-2023             #
############################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(rgdal)
library(sf)
library(plotly)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

df = read.csv("ERA5_Block_1pixel_1979_2020.csv", sep = ",")

#Shapes
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")

xingu <- readOGR("Shapes/Xingu_MT.shp")
xingu = st_as_sf(xingu)

bio = readOGR("Shapes/Limit_bioma_xingu.shp")
bio = st_as_sf(bio)

#Wind Speed at 0.9 quantile ---------------------------------------------------
quantile(df$Wind, probs = 0.9)

dens = ggplot(df, aes(x = Wind))+
  geom_density(fill = "red", alpha = 0.7)+
  geom_vline(xintercept =  10.17, col = "blue")+
  annotate(geom = "text", x = 12, y = 0.2, col = "blue", label = "0.9 quantile")+
  labs(y = NULL, x = "Wind Gust (m/s)", title = "Dialy Maximum Wind Gust")+
  theme_bw();dens

ggsave(filename = "Extreme_WG_minimal_hist_1979_2020.png", plot = dens,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 13, height = 8, units = "cm", dpi = 300)

#Count Wind Gust higher than 11.72 --------------------------------------------
df = df %>% 
  filter(window == "1d") %>% 
  separate(xy, c("x","y"), sep = "_")

df$x = as.numeric(df$x)
df$y = as.numeric(df$y)


df2 = df %>% 
  group_by(x, y) %>%
  filter(Wind >= 10.17) %>% 
  ungroup()

df3 = df2 %>% 
  group_by(x, y) %>% 
  summarise(Recurrence = length(Wind))

#Map of extreme Wind Gust Recurrence ------------------------------------------
#Area 1 point
pont = df2 %>%
  filter(x == -52.25) %>%
  filter(y == -13)
#Small adjusts
pont$x = pont$x + 0.125
pont$y = pont$y - 0.125


a = ggplot(df3)+
  geom_raster(aes(x, y, fill = Recurrence))+
  geom_point(data = pont, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  geom_sf(data = xingu, colour = "black", fill = NA, size = 1, stroke = 2)+
  geom_sf(data = bio, colour = "yellow", fill = NA, size = 1, stroke = 2, linetype = "21")+
  coord_sf()+
  labs(x=NULL, y=NULL, title = "Extreme Wind minimal window (1979-2020)")+
  scale_fill_viridis(option = "magma",
                     direction = -1, limits=c(200, 900),
                     name = "Wind > 10 m/s")+
  theme_minimal()+
  theme(text = element_text(size = 14)); a



ggsave(filename = "Xingu_Extreme_WG_minimal_1979_2020.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 20, height = 16, units = "cm", dpi = 300)

#setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
#write.csv(df3, "Extreme_WG_Recurrence.csv", sep = ",", row.names = F)

#Other atempts --------------------------------------------------------------
df4 = df2
df4$date = substr(df4$date, 1, 4)

ggplot(df4, aes(x = date))+
  geom_bar()


df4 = df4 %>% 
  group_by(date) %>% 
  summarise(Recurrence = length(Wind))
df4$grp = c("a")  

ggplot(df4, aes(x = date, y = Recurrence))+
  geom_line(aes(group = grp))











