#Extrating Mapbiomes Classes by Acara Basin

#Eduardo Q Marques 24-02-2026

library(terra)
library(sf)
library(tidyverse)

#Data
mb = rast("G:/My Drive/Research/Geodata/Rasters/MapBiomes_Brazil/Collection_9/mb2023.tif")
bc = read_sf("G:/My Drive/Research/Propostas/Ativas/FINEP_2025/Bacias_Acara/Bacias_Level_12_Acara_Guama/Bacias_Level_12_Acara_Guama.shp")

plot(mb)
plot(bc, add = T)

mb2 = mask(crop(mb, bc), bc)
plot(mb2)

#Dataframe to input classes
df = freq(mb2)

df$area_ha = (df$count*900)/10000
df$Bacia = "All"

#By Basin
for (i in 1:731) {
  print(bc$HYBAS_ID[i])
  mbi = mask(crop(mb, bc[i,2]), bc[i,2])
  plot(mbi)
  
  df2 = freq(mbi)
  df2$area_ha = (df2$count*900)/10000
  df2$Bacia = bc$HYBAS_ID[i]
  
  df = rbind(df, df2)
}

