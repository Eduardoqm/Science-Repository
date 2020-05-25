#Cut Hyperion stcks for the mean area
#Eduardo Q Marques
#28/08/2019

library(raster)
library(rgdal)
#Reference vector to crop
area1 = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Area1big")

#Image to crop
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion")
a <- stack("Reg_27_july_2005.tif")

#Change vector extent and crop
area1 = spTransform(area1, crs(a))
a <- crop(a, area1)

#Salve cropped image
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")
writeRaster(a, filename = 'Reg_27_july_2005', format = "GTiff", bylayer = FALSE)