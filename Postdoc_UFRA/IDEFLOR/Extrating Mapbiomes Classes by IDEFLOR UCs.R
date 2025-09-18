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

idclas = unique(mb2)
idclas$npixel = 0

#freq(mb2 == 0)[2,3]

for (z in 1:18) {
  idclas[z,2] <- freq(mb2 == idclas[z,1])[2,3]
}
