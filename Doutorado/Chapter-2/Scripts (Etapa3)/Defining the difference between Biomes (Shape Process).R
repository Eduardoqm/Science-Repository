#Defining the difference between Biomes

#Eduardo Q Marques 23-10-2022

library(raster)
library(rgdal)

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/Blocked_ERA5SL/")
era5 = brick("ERA_SL_WG_block_7pixels_2012_2020.nc")
era5 = era5[[1]]
plot(era5)


setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data")
bioma <- readOGR("Shapes/biomas.shp")
xingu <- readOGR("Shapes/Xingu_MT.shp")

plot(bioma, add= T)
plot(xingu, add = T)

era5b = crop(era5, xingu)
plot(era5b)
plot(bioma, add= T)
plot(xingu, add = T)

#Cerrado
plot(bioma[3,]) #Number of Cerrado polygon

era5c = mask(era5b, bioma[3,])
plot(era5c)
plot(bioma, add= T)
plot(xingu, add = T)

era_grid <- rasterToPolygons(era5c, fun=function(x){x>=0})
plot(era_grid, add = T)

df_cerrado = extract(era5b, era_grid, df = T)
df_cerrado$coord = coordinates(era_grid)
df_cerrado$Bioma = c("Cerrado")

cerrado = df_cerrado[,c(-1,-2)]


#Amazonia
plot(bioma[1,]) #Number of Cerrado polygon

era5c = mask(era5b, bioma[1,])
plot(era5c)
plot(bioma, add= T)
plot(xingu, add = T)

era_grid <- rasterToPolygons(era5c, fun=function(x){x>=0})
plot(era_grid, add = T)

df_amaz = extract(era5b, era_grid, df = T)
df_amaz$coord = coordinates(era_grid)
df_amaz$Bioma = c("Amazonia")

amaz = df_amaz[,c(-1,-2)]

#Export result
library(tidyverse)

df = rbind(cerrado, amaz)

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
write.csv(df, "Biome_coord.csv", row.names = F)

















