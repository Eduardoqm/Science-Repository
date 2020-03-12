#Make all Landsat bands data as data frame with XY

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

#Data Bank
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")

train <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="train")

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")

hy = brick('Reg_09_Aug_2004.tif')
hy04 = crop(hy, area1)

hy = brick('Reg_27_july_2005.tif')
hy05 = crop(hy, area1)

hy = brick('Reg_02_Aug_2006.tif')
hy06 = crop(hy, area1)

hy = brick('Reg_22_july_2008.tif')
hy08 = crop(hy, area1)

hy = brick('Reg_22_july_2010.tif')
hy10 = crop(hy, area1)

hy = brick('Reg_24_july_2011.tif')
hy11 = crop(hy, area1)

hy = brick('Reg_20_july_2012.tif')
hy12 = crop(hy, area1)

#hy04@data = hy11@data;hy05@data = hy11@data;hy06@data = hy11@data
#hy08@data = hy11@data;hy10@data = hy11@data;hy12@data = hy11@data

#Supervised to one year
#Classes(Intact Forest, Grass, Soil, Initial Regeneration, intermediate Regeneration)

class11 <- superClass(normImage(hy11), 
                      trainData = train, 
                      responseCol = "CLASS", model = "rf", 
                      tuneLength = 1, trainPartition = 0.7)

plot(class11$map)
class11$model


###
###
###
#Using class to predict other classifications
hy12 <- resample(hy12,hy11,method='bilinear')

pred_12 <- predict(normImage(hy12), class11$map)
#pred_12 <- predict(class11, normImage(hy12))
#pred_11 <- predict(class11, normImage(hy11))
pred_10 <- predict(class11, normImage(hy10))
pred_08 <- predict(class11, normImage(hy08))
pred_06 <- predict(class11, normImage(hy06))
pred_04 <- predict(class11, normImage(hy04))
pred_05 <- predict(class11, normImage(hy05))

levelplot(pred_04, margin = F, main = '2004')
levelplot(pred_05, margin = F, main = '2005')
levelplot(pred_06, margin = F, main = '2006')
levelplot(pred_08, margin = F, main = '2008')
levelplot(pred_10, margin = F, main = '2010')
levelplot(class11$map, margin = F, main = '2011')
levelplot(pred_12, margin = F, main = '2012')

rasterVis::levelplot(stack(pred_04, pred_06, pred_08))

f_dudu <- function(x, yr)as.data.frame(x) %>% as.tbl() %>% count(layer) %>% mutate(Yr = yr)

class_data <- rbind(f_dudu(pred_04, yr = 2004),
                    f_dudu(pred_05, yr = 2005),
                    f_dudu(pred_06, yr = 2006),
                    f_dudu(pred_08, yr = 2008),
                    f_dudu(pred_10, yr = 2010),
                    f_dudu(class11$map, yr = 2011),
                    f_dudu(pred_12, yr = 2012))

class_data %>%
  ggplot(aes(x = as.factor(Yr), 
             y = n, fill = as.factor(layer))) +
  geom_col()

class_data %>%
  ggplot(aes(x = Yr, 
             y = n, fill = as.factor(layer))) +
  geom_area()
