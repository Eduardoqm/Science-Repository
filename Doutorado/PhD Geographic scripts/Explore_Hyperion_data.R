library(raster)
library(rasterVis)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion")
a <- stack("Reflectance_22_July_2008")
a = a/1000

levelplot(a[[4]], margin = FALSE, par.settings = RdBuTheme)

for (x in 1:152) {
  print(a[[x]]@data@names)
}

p864 = a[[44]]

p660 = a[[24]]

ndvi = (p864-p660)/(p864+p660)

levelplot(ndvi, margin = FALSE, par.settings = RdBuTheme)

#writeRaster(ndvi, filename="ndvi2008.tiff", overwrite=TRUE)
