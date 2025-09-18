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
idclas = unique(mb2)
idclas$npixel = 0

#freq(mb2 == 0)[2,3]

for (z in 1:18) {
  print(idclas[z,1])
  idclas[z,2] <- freq(mb2 == idclas[z,1])[2,3]
}

idclas$area_ha = (idclas$npixel*900)/10000
idclas$UC = "All"

#By UCs
mbi = mask(crop(mb, ucs[1,2]), ucs[1,2])
idclas_a = idclas
idclas_b = idclas

for (z in 1:18) {
  print(idclas[z,1])
  idclas_a[z,2] <- freq(mb2 == idclas_a[z,1])[2,3]
  idclas_a$area_ha = (idclas_a$npixel*900)/10000
}
idclas_a$UC = ucs$nome_[1]
idclas_a = rbind(idclas_a, idclas_b)



for (i in 2:3) {
  ucs$nome_[i]
  mbi = mask(crop(mb, ucs[i,2]), ucs[i,2])
  for (z in 1:18) {
    print(idclas[z,1])
    idclas_b[z,2] <- freq(mb2 == idclas[z,1])[2,3]
    idclas_b$area_ha = (idclas$npixel*900)/10000
    idclas$UC = ucs$nome_[i]
  }
}














