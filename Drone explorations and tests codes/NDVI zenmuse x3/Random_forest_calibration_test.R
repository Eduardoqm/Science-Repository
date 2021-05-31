library(randomForest)
library(sf)
library(sp)
library(tidyverse)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)
library(ggplot2)

#Data Bank ====================================================================================
setwd("C:/Users/Eduardo Q Marques/Downloads/Imagens de drone tanguro/imagens para testes/Soja")

calib = brick(choose.files())#Image in tiff processed by MPAIR
no_calib = brick(choose.files())#Image in jpeg not processed by MPAIR

img = brick(choose.files())#Image from dorne flu in jpeg to calibration

#Change name of bands (if did not, return in error)
names(calib) <- paste0(rep('band', nlayers(calib)), 1:nlayers(calib))
names(no_calib) <- paste0(rep('band', nlayers(no_calib)), 1:nlayers(no_calib))
names(img) <- paste0(rep('band', nlayers(img)), 1:nlayers(img))

x11()

plot(no_calib[[1]])
plot(calib[[1]])

#Sample polygon
amostra <- drawPoly()

plot(calib[[3]])
plot(amostra, add=T)

#Crop samples
cal_sample <- crop(calib, amostra)
no_cal_sample <- crop(no_calib, amostra)
plot(cal_sample)
plot(no_cal_sample)

##Traine Data =================================================================================
cal_df = as.data.frame(cal_sample)
no_cal_df = as.data.frame(no_cal_sample)

#Band 1
b1train <- as.data.frame(no_cal_df)
b1train$cal <- cal_df$band1
names(b1train)[ncol(b1train)] <- "class"
b1train$class <- as.factor(b1train$class)

#Band 3
b3train <- as.data.frame(no_cal_df)
b3train$cal <- cal_df$band3
names(b3train)[ncol(b3train)] <- "class"
b3train$class <- as.factor(b3train$class)

#Create Random Forest Model ===================================================================
#Band 1
b1_mdl <- randomForest(b1train$class ~., data = b1train)
#getTree(b1_mdl, k=1)
varImpPlot(b1_mdl, col='darkgreen')#Important bands to class

#Band 3
b3_mdl <- randomForest(b3train$class ~., data = b3train)
#getTree(b3_mdl, k=1)
varImpPlot(b3_mdl, col='darkgreen')#Important bands to class


#Classification target to test accuracy =======================================================
testb1 <- raster::predict(no_cal_sample, b1_mdl, progress = "text", type = "response")
testb3 <- raster::predict(no_cal_sample, b3_mdl, progress = "text", type = "response")

#B1 accuracy
plot(testb1$layer, cal_sample$band1)
plot(stack(cal_sample[[1]], testb1))
boxplot(testb1)
boxplot(cal_sample[[1]])

#B3 accuracy
plot(testb3$layer, cal_sample$band3)
plot(stack(cal_sample[[3]], testb3))

#Do calibration with RF models ================================================================
b1 <- raster::predict(img, b1_mdl, progress = "text", type = "response")
b3 <- raster::predict(img, b3_mdl, progress = "text", type = "response")

plot(stack(b1, img[[1]]))
plot(stack(b3, img[[3]]))



#Stack result ==================================================================================
img_calib = stack(b1, b3)


ndvi = (img_calib[[2]]-img_calib[[1]])/(img_calib[[2]]+img_calib[[1]])
levelplot(ndvi, margin = F, main = "NDVI calibrated by RF Soja",
          col.regions = viridis(100))


#Export image ==================================================================================
#writeRaster(class04, "class_2004.tif", overwrite = TRUE)
#writeRaster(class05, "class_2005.tif", overwrite = TRUE)
#writeRaster(class06, "class_2006.tif", overwrite = TRUE)
#writeRaster(class08, "class_2008.tif", overwrite = TRUE)
#writeRaster(class10, "class_2010.tif", overwrite = TRUE)
#writeRaster(class11, "class_2011.tif", overwrite = TRUE)
#writeRaster(class12, "class_2012.tif", overwrite = TRUE)




