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

mb3 = ifel(mb2 < 11, 1, mb2)
mb2 = ifel(mb2 > 20, 3, 2)
plot(mb3)

df = freq(mb2)

df$area_ha = (df$count*900)/10000
df$UC = "All"

#By UCs
for (i in 1:29) {
  print(ucs$nome_[i])
  mbi = mask(crop(mb2, ucs[i,2]), ucs[i,2])
  plot(mbi)
  
  df2 = freq(mbi)
  df2$area_ha = (df2$count*900)/10000
  df2$UC = ucs$nome_[i]
  
  df = rbind(df, df2)
}

#Input classes names
df = df[,-1]
colnames(df)[1] = c("class")
#df = df %>% dplyr::filter(class != 0)

df$class[df$class == 1] = "1-10 years"
df$class[df$class == 2] = "11-20 years"
df$class[df$class == 3] = "> 20 years"


setwd("G:/Meu Drive/Postdoc_UFRA/Colaborations/IDEFLOR")
write.csv(df, "IDEFLOR_Area_MB_Secondary_Forest.csv", row.names = F)
