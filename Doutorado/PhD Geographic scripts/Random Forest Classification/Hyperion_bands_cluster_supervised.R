#Hyperion supervised calssification (Random Forest)

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

hy11 = brick('Reg_24_july_2011.tif')
#hy11 = crop(hy, area1)

hy = brick('Reg_20_july_2012.tif')
hy12 = crop(hy, area1)

#hy04@data = hy11@data;hy05@data = hy11@data;hy06@data = hy11@data
#hy08@data = hy11@data;hy10@data = hy11@data;hy12@data = hy11@data

#Supervised to one year
#Classes(Intact Forest, Grass, Soil, Initial Regeneration, intermediate Regeneration)
traine = spTransform(traine, crs(hy11[[1]]))
hy11F <-  approxNA(hy11)
class11B <- superClass(hy11F, trainData = traine, 
                       responseCol = "class",
                       model = "rf", nSamples = 1,
                       tuneLength = 3, trainPartition = 0.6,
                       predict = TRUE, predType = "prob")


levelplot(class11B$map, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)))
class11B$model


ff <- class11B$map
ff[ff>0.5] = 1
ff[ff<=0.5] = 0
levelplot(ff, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(2)))


###
###
###
#Using class to predict other classifications
hy12 <- resample(hy12,hy11,method='bilinear')

pred_12 <- raster::predict(class11$map, hy12)
calc12 <- calc(class11$map,predict)


names(hy12) = names(hy10) = names(hy11)
pred_12 <- predict(class11B, hy12)
plot(pred_12)
#pred_11 <- predict(class11, normImage(hy11))
pred_10 <- predict(class11B, hy10)
plot(pred_10)
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
