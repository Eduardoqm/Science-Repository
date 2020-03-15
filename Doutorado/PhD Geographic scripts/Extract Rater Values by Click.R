#Extract Rater Values by Click

#Eduardo Q Marques 11-03-2020

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
#area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")

hy04 = brick('Reg_09_Aug_2004.tif')

hy05 = brick('Reg_27_july_2005.tif')

hy06 = brick('Reg_02_Aug_2006.tif')

hy08 = brick('Reg_22_july_2008.tif')

hy10 = brick('Reg_22_july_2010.tif')

hy11 = brick('Reg_24_july_2011.tif')
#hy11 = crop(hy, area1)

hy12 = brick('Reg_20_july_2012.tif')


x11()
plot(hy11[[9]], col=viridis(10))
#levelplot(hy11[[7]], margin = F)

#Classes(1-Intact Forest, 2-Grass, 3-Soil, 4-Initial Regeneration, 5-intermediate Regeneration)
intact = data.frame(click(hy11, n=Inf, id=TRUE, xy=TRUE, cell=FALSE, type="n", show=FALSE))
intact$class = c(1)

grass = data.frame(click(hy11, n=Inf, id=TRUE, xy=TRUE, cell=FALSE, type="n", show=FALSE))
grass$class = c(2)

soil = data.frame(click(hy11, n=Inf, id=TRUE, xy=TRUE, cell=FALSE, type="n", show=FALSE))
soil$class = c(3)

ini_reg = data.frame(click(hy11, n=Inf, id=TRUE, xy=TRUE, cell=FALSE, type="n", show=FALSE))
ini_reg$class = c(4)

inter_reg = data.frame(click(hy11, n=Inf, id=TRUE, xy=TRUE, cell=FALSE, type="n", show=FALSE))
inter_reg$class = c(5)

df = rbind(intact, grass, soil, ini_reg, inter_reg)

df$year = c(2011)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Classification")
write.csv(df, "train_2011.csv", sep = ',')

