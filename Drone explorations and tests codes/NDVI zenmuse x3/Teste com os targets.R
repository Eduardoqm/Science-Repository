library(raster)
library(rasterVis)
library(rgdal)
library(viridis)
library(ggplot2)

#Test targets
setwd("C:/Users/Eduardo Q Marques/Downloads/Imagens de drone tanguro/imagens para testes")
dir()

raw_img = brick(choose.files())
jpeg_img = brick(choose.files())
calib_img = brick(choose.files())

x11()
plot(raw_img)

x11()
plot(jpeg_img)

x11()
plot(calib_img)

b3 = stack(jpeg_img[[3]], calib_img[[3]])
x11()
plot(b3)


#Transform in df to plot data
df = as.data.frame(b3, xy = T)
colnames(df) = c("x", "y", "jpeg", "calibrated")
View(df)

ggplot(df, aes(x = jpeg, y = calibrated))+
  geom_smooth()
  
  
  
  
  
  
  