#Extrating Secindary Forest age by IDEFLOR UCs

#Eduardo Q Marques 23-09-2025

library(terra)
library(sf)
library(tidyverse)

#Data
mb = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/MB_Forest_age_30m.tif")
ucs = read_sf("G:/Meu Drive/Postdoc_UFRA/Colaborations/IDEFLOR/Shapefile_UCs_Estaduais_IDEFLOR-Bio.shp")

plot(mb)
plot(ucs, add = T)

mb2 = mask(crop(mb, ucs), ucs)
plot(mb2)

#Input classes
#1 = 1-10 years
#1 = 11-20 years
#1 = > 20 years

mb2 = ifel(mb2 < 11, 1, mb2)
mb2 = ifel(mb2 > 20, 3, 2)
plot(mb2)

df = freq(mb2)

df$area_ha = (df$count*900)/10000
df$UC = "All"

#By UCs
for (i in 1:29) {
  print(ucs$nome_[i])
  mbi = mask(crop(mb, ucs[i,2]), ucs[i,2])
  plot(mbi)
  
  df2 = freq(mbi)
  df2$area_ha = (df2$count*900)/10000
  df2$UC = ucs$nome_[i]
  
  df = rbind(df, df2)
}

#Input classes names
df = df[,-1]
colnames(df)[1] = c("class")
df = df %>% dplyr::filter(class != 0)

df$class[df$class == 3] = "Forest Formation"
df$class[df$class == 4] = "Savanna Formation"
df$class[df$class == 5] = "Mangrove"
df$class[df$class == 6] = "Floodable Forest"
df$class[df$class == 11] = "Wetland"
df$class[df$class == 12] = "Grassland"
df$class[df$class == 29] = "Rocky Outcrop"
df$class[df$class == 15] = "Pasture"
df$class[df$class == 39] = "Soybean"
df$class[df$class == 40] = "Rice"
df$class[df$class == 41] = "Other Temporary Crops"
df$class[df$class == 9] = "Forest Plantation"
df$class[df$class == 23] = "Beach, Dune and Sand Spot"
df$class[df$class == 24] = "Urban Area"
df$class[df$class == 30] = "Mining"
df$class[df$class == 25] = "Other non Vegetated Areas"
df$class[df$class == 26] = "Water"
df$class[df$class == 33] = "River, Lake and Ocean"


setwd("G:/Meu Drive/Postdoc_UFRA/Colaborations/IDEFLOR")
write.csv(df, "IDEFLOR_Area_MB_Classes.csv", row.names = F)
