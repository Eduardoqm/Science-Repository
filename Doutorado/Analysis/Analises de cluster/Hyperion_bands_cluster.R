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


set.seed(25)
class4 <- unsuperClass(hy04, nSamples = 1000, nClasses = 8, nStarts = 25,
                       norm = TRUE, clusterMap = TRUE)
plot(class4$map)
class4$model

class5 <- unsuperClass(hy05, nSamples = 1000, nClasses = 6, nStarts = 25)
plot(class5$map)
class5$model

class6 <- unsuperClass(hy06, nSamples = 1000, nClasses = 6, nStarts = 25)
plot(class6$map)
class6$model

class8 <- unsuperClass(hy08, nSamples = 1000, nClasses = 8, nStarts = 25,
                       norm = FALSE, clusterMap = FALSE)
plot(class8$map)
class8$model

class10 <- unsuperClass(hy10, nSamples = 1000, nClasses = 6, nStarts = 25)
plot(class10$map)
class10$model

class11 <- unsuperClass(normImage(hy11), 
                                  nSamples = 1000, nClasses = 6, nStarts = 25,
                        norm = F, clusterMap = FALSE)
plot(class11$map)
class11$model

class12 <- unsuperClass(hy12, nSamples = 1000, nClasses = 6, nStarts = 25)
plot(class12$map)
class12$model

###
###
###

pred_12 <- predict(class11, normImage(hy12))
pred_11 <- predict(class11, normImage(hy11))
pred_10 <- predict(class11, normImage(hy10))
pred_08 <- predict(class11, normImage(hy08))
pred_06 <- predict(class11, normImage(hy06))
pred_05 <- predict(class11, normImage(hy05))
pred_04 <- predict(class11, normImage(hy04))

levelplot(pred_04, margin = F, main = '2004')
levelplot(pred_05, margin = F, main = '2005')
levelplot(pred_06, margin = F, main = '2006')
levelplot(pred_08, margin = F, main = '2008')
levelplot(pred_10, margin = F, main = '2010')
levelplot(pred_11, margin = F, main = '2011')
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
