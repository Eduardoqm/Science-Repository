#Hyperion supervised calssification (Random Forest)
#Version-1.2
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
library(RStoolbox)
library(caret)
library(randomForest)
library(e1071)

#Data Bank
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")

#traine <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="train")

traine <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Classification",layer="classpoint_2011")

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")

hy11 = brick('Reg_24_july_2011.tif')
#hy11 = crop(hy, area1)

#Supervised to one year
#Classes(Intact Forest, Grass, Soil, Initial Regeneration, intermediate Regeneration)
traine = spTransform(traine, crs(hy11[[1]]))
hy11F <-  approxNA(hy11)
class11B <- superClass(hy11F, trainData = traine, 
                       responseCol = "class",
                       model = "rf", nSamples = 1,
                       tuneLength = 3, trainPartition = 0.6,
                       predict = TRUE)

plot(class11B$map, col = viridis(10))
levelplot(class11B$map)
levelplot(class11B$map, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)))
class11B$model


ff <- class11B$map
ff[ff>0.5] = 1
ff[ff<=0.5] = 0
plot(ff, col = viridis(2))

class11C = crop(class11B$map, area1)
ff2 = crop(ff, area1)

levelplot(class11C, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)))

levelplot(ff2, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(2)))





cl = as.data.frame(class11C, xy = TRUE)
colnames(cl) = c('x','y','value')
cl$value = as.integer(cl$value)


ggplot(cl, aes(x,y, fill = value))+
  geom_raster()+
  scale_fill_viridis()

library(rayshader)

g3d = ggplot(cl, aes(x,y, fill = value))+
  geom_raster()+
  scale_fill_viridis()

plot_gg(g3d,multicore=TRUE,width=10,height=5,scale=100,windowsize=c(1000,500),
        zoom = 0.55, phi = 30)
