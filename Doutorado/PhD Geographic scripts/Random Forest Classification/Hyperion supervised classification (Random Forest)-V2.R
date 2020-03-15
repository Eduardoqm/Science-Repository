#Hyperion supervised calssification (Random Forest)
#Version-2
#Eduardo Q Marques 15-03-2020

library(randomForest)
library(sf)
library(sp)
library(tidyverse)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)

#Hyperion data
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")

img4 = brick('Reg_09_Aug_2004.tif')
img5 = brick('Reg_27_july_2005.tif')
img6 = brick('Reg_02_Aug_2006.tif')
img8 = brick('Reg_22_july_2008.tif')
img10 = brick('Reg_22_july_2010.tif')
img11 <- brick('Reg_24_july_2011.tif')#Font of traine data (2011)
img12 = brick('Reg_20_july_2012.tif')

#plotRGB(img11, 100, 50, 20, stretch = 'hist')

#Change name of bands (if did not, return in error)
names(img4) <- paste0(rep('band', nlayers(img4)), 1:nlayers(img4))
names(img5) <- paste0(rep('band', nlayers(img5)), 1:nlayers(img5))
names(img6) <- paste0(rep('band', nlayers(img6)), 1:nlayers(img6))
names(img8) <- paste0(rep('band', nlayers(img8)), 1:nlayers(img8))
names(img10) <- paste0(rep('band', nlayers(img10)), 1:nlayers(img10))
names(img11) <- paste0(rep('band', nlayers(img11)), 1:nlayers(img11))
names(img12) <- paste0(rep('band', nlayers(img12)), 1:nlayers(img12))

#Sample points
amostras <- read_sf("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Classification/classpoint_2011.shp")
amostras = st_transform(amostras, crs(img11[[1]]))
plot(amostras, add=T)


# ExtraÃ§Ã£o de dados: ----
valsTrain <- raster::extract(img11, amostras)

head(valsTrain)

valsTrain <- data.frame(valsTrain, amostras$class)
head(valsTrain)
names(valsTrain)[ncol(valsTrain)] <- "class"

valsTrain$class <- as.factor(valsTrain$class)
class(valsTrain$class)

# Criando modelo randomForest ----
rf.mdl <- randomForest(valsTrain$class ~., data = valsTrain)
rf.mdl

getTree(rf.mdl, k=1)

varImpPlot(rf.mdl, col='darkgreen')#Important bands to class

#Classification
class04 <- raster::predict(img4, rf.mdl, progress = "text", type = "response")
class05 <- raster::predict(img5, rf.mdl, progress = "text", type = "response")
class06 <- raster::predict(img6, rf.mdl, progress = "text", type = "response")
class08 <- raster::predict(img8, rf.mdl, progress = "text", type = "response")
class10 <- raster::predict(img10, rf.mdl, progress = "text", type = "response")
class11 <- raster::predict(img11, rf.mdl, progress = "text", type = "response")
class12 <- raster::predict(img12, rf.mdl, progress = "text", type = "response")

#Classification Maps
levelplot(class04, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2004")

levelplot(class05, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2005")

levelplot(class06, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2006")

levelplot(class08, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2008")

levelplot(class10, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2010")

levelplot(class11, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2011")

levelplot(class12, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "2012")

#Save class raster
#writeRaster(rf.class, "rf_ClassificationCrossValidation.tif", overwrite = TRUE)
