#Extract Raster Values by Points

#Eduardo Q Marques 12-03-2020

library(tidyverse)
library(ggridges)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(rasterVis)


#Data Bank
points <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Classification",layer="classpoint_2011")

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")

hy04 = brick('Reg_09_Aug_2004.tif')

hy05 = brick('Reg_27_july_2005.tif')

hy06 = brick('Reg_02_Aug_2006.tif')

hy08 = brick('Reg_22_july_2008.tif')

hy10 = brick('Reg_22_july_2010.tif')

hy11 = brick('Reg_24_july_2011.tif')
#hy11 = crop(hy, area1)

hy12 = brick('Reg_20_july_2012.tif')

points = spTransform(points, crs(hy11[[1]]))


#Classes(1-Intact Forest, 2-Grass, 3-Soil, 4-Initial Regeneration, 5-intermediate Regeneration)
#Data to add in sample
dat = as.data.frame(points)
dat = dat[,c(-1)]
colnames(dat) = c('class','x','y')

#Extract class
class = as.data.frame(extract(hy11, points))
class = cbind(class,dat)

class$year = c(2011)



#Sava classes
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Classification")
write.csv(class, "trainpoint_2011.csv", sep = ',')

