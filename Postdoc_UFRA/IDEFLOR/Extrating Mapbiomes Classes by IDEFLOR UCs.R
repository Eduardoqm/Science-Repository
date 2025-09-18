#Extrating Mapbiomes Classes by IDEFLOR UCs

#Eduar Q Marques 18-09-2025

library(terra)
library(sf)


mb = rast("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/Collection_9/mb2023.tif")
ucs = read_sf("G:/Meu Drive/Postdoc_UFRA/Colaborations/IDEFLOR/Shapefile_UCs_Estaduais_IDEFLOR-Bio.shp")

plot(mb)
plot(ucs, add = T)

mb2 = mask(crop(mb, ucs), ucs)
plot(mb2)

classes = unique(mb2)[,1]

freq(mb2 == 41)[2,3]
