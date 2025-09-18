#Extrating Mapbiomes Classes by IDEFLOR UCs

#Eduardo Q Marques 18-09-2025

library(terra)
library(sf)

#Data
mb = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/Collection_9/mb2023.tif")
ucs = read_sf("G:/Meu Drive/Postdoc_UFRA/Colaborations/IDEFLOR/Shapefile_UCs_Estaduais_IDEFLOR-Bio.shp")

plot(mb)
plot(ucs, add = T)

mb2 = mask(crop(mb, ucs), ucs)
plot(mb2)

#Dataframe to input classes
df = freq(mb2)

df$area_ha = (df$count*900)/10000
df$UC = "All"

#By UCs
mbi = mask(crop(mb, ucs[1,2]), ucs[1,2])

df2 = freq(mbi)
df2$area_ha = (df2$count*900)/10000
df2$UC = ucs$nome_[1]


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













