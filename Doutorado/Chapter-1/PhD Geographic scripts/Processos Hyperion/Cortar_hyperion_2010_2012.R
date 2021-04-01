#Cut 2010 and 2012 edge scene

#Eduardo Q Marques 31-03-2021

library(raster)
library(rgdal)

#Criar lista com as bandas
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyper corte 2010-2012")
s2010 = stack(files1 <- list.files(pattern="2010.tif$")) 
s2012 = stack(files2 <-list.files(pattern = "2012.tif$"))

corte <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Corte_2010_2012")

s2010b = crop(s2010, corte)
