#Cut 2010 and 2012 edge scene

#Eduardo Q Marques 31-03-2021

library(raster)
library(rgdal)

#Rasters stacks
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyper sem corte 2010-2012")
s2010 = stack(files1 <- list.files(pattern="2010.tif$")) 
s2012 = stack(files2 <-list.files(pattern = "2012.tif$"))

#Shape to cut edge
corte <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Corte_2010_2012")

#Cutting 2010
s2010b = crop(s2010, corte)
s2010b = mask(s2010b, corte)

#Cutting 2012
s2012b = crop(s2012, corte)
s2012b = mask(s2012b, corte)

#Export images
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyper sem corte 2010-2012/After_cut")

for (i in 1:length(s2010b)) {
  writeRaster(s2010b[[i]], paste(names(s2010b[[i]]), 2010, sep = "-", collapse = NULL), format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(s2012b)) {
  writeRaster(s2012b[[i]], paste(names(s2012b[[i]]), 2012, sep = "-", collapse = NULL), format = "GTiff", bylayer = FALSE)
}






